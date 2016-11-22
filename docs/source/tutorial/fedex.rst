.. _fedex:

Running AG on AWS EC2
---------------------

Federating multiple repositories located on the same server (as shown
in :ref:`example16`) can be useful for organizing data, but to truly
explore the scalability potential of federated repositories we should
look at a system consisting of multiple machines. This example will
illustrate this by instantiating multiple AllegroGraph virtual
machines on `AWS`_, joining all servers in a single federated
repository and issuing a simple query.

.. warning:: This example involves instantiating multiple AWS
             resources. These are *not* free and you *will* be charged
             by Amazon. While the cost should not exceed a few
             dollars, we are not responsible for any charges you might
             incur while running this tutorial.

To allow our script to interact with AWS we will need to install an
additional dependency - `boto3`_.

.. code-block:: sh

   pip install boto3

We will also need to configure credentials to allow the script to
access the Amazon account that it should use. This can be done in
|multiple ways: credentials|. One possibility is to create a file
named ``~/.aws/credentials`` with the following contents:

.. code-block:: ini

   [default]
   aws_access_key_id = YOUR_KEY
   aws_secret_access_key = YOUR_SECRET

The key and key id can be obtained from the web interface of `AWS`_.

We're now ready to begin writing the actual script. Let's start with
some imports:

.. testsetup:: -fedex

   import logging
   logging.getLogger("requests.packages.urllib3.connectionpool").setLevel(logging.WARNING)

.. testcode:: -fedex

   import boto3
   import atexit
   import time

We will need the ``boto3`` module to interact with AWS. We also want
to make sure that any resources we have provisioned will be properly
discarded when the scripts exits, even if it exits because of an
error. The ``atexit`` module provides a convenient way of doing that.

.. warning:: While the mechanism of releasing resources used in this
             tutorial is reasonably robust, it might still fail in
             rare circumstances (e.g. if your computer loses power).
             It is important to manually check your AWS account for
             any stray resources after executing this tutorial to
             avoid unnecessary costs.

Now we will set a few configuration constants:

.. testcode:: -fedex

   REGION = 'us-west-2'
   AMI = 'ami-5616d82e'
   TYPE = 't2.medium'

These describe the kind of servers that we will be provisioning, as
well as the AWS `region`_ in which our resources will reside. `AMI`
should be the image id of an AllegroGraph image appropriate for the
chosen region. Image identifiers can be found |here: ag-ami|. The
actual value shown above refers to the AllegroGraph 6.3.0 HVM image
for the ``us-west-2`` region. ``TYPE`` is the 'instance type' and
determines the amount of compute resources (RAM and CPU) that will be
available for each of our servers. The list of instance types can be
found |here: instance-types|.

We will create a specified number of servers plus an additional one to
manage the federation. The number below is the number of federated
instances (i.e. it does *not* include the management instance).

.. testcode:: -fedex

   INSTANCE_COUNT = 2

It is desirable to limit the connections to the system that we are
creating so that only your machine will be able to access it. If you
have a static IP address, fill it below. If not, leave the value as it
is.

.. testcode:: -fedex

   MY_IP = '0.0.0.0/0'

The next set of constants describe various aspect of the
infrastructure that we are about to provision. There should be no need
to adjust any values here.
   
.. testcode:: -fedex

   VPC_CIDR = '10.0.0.0/16'
   PUBLIC_CIDR = '10.0.0.0/24'
   PRIVATE_CIDR = '10.0.1.0/24'
   KEY_NAME = 'AG-TUTORIAL-KEYPAIR'
   PROFILE_NAME = 'agprofile'
   USER = 'ec2-user'

The first three values describe the structure of the `virtual network`_
that will contain our machines. The network will consist of two
subnets - a public one which will contain the management instance and
a private one which will contain all other servers. Only machines in
the public subnet will be directly accessible over the
Internet. ``KEY_NAME`` is a name that will be assigned to the
`keypair`_ that we will generate and use to connect to our
servers. ``PROFILE_NAME`` is the name for an `instance profile`_
that we will create for our AG servers. ``USER`` is the system user on
the machines we want to connect to. Since AllegroGraph's images are
based on Amazon Linux, that username must be set to ``ec2_user``.

We will now create handles for Amazon services that we want to use.

.. testcode:: -fedex

   ec2 = boto3.resource('ec2', region_name=REGION)
   ec2_client = boto3.client('ec2', region_name=REGION)
   iam = boto3.resource('iam', region_name=REGION)
   ssm = boto3.client('ssm', region_name=REGION)

These come in two variants, called ``resources`` and
``clients``. Clients provide low-level access to the Amazon API, while
resources are a high-level abstraction built over clients. Using
resources is slightly easier and thus preferable, but some operations
can only be performed with clients. In our case ``ec2_client`` and
``ssm`` objects are clients for the respective AWS services, while
other handles are resources.

The first thing that we will create is a `security role`_ that will be
assigned to our machines. This is necessary to allow the use of
`SSM`_, which we will need to execute scripts.

.. testcode:: -fedex

   ssm_role = iam.create_role(RoleName='ssm',
                              AssumeRolePolicyDocument="""{
       "Version":"2012-10-17",
       "Statement":[
          {
             "Effect":"Allow",
             "Principal":{
                "Service": ["ssm.amazonaws.com", "ec2.amazonaws.com"]
             },
             "Action":"sts:AssumeRole"
          }]}""")
   atexit.register(ssm_role.delete)

The role created above can be assumed by EC2 instances and allows
access to SSM. We have also installed an ``atexit`` handler to make
sure that the role is deleted once we are done. We will do the same
thing for every other AWS resource that we create.

To make all instances that have assumed the role defined above
accessible to SSM we need to connect that role to the appropriate
policy document.

.. testcode:: -fedex

   ssm_arn = 'arn:aws:iam::aws:policy/service-role/AmazonEC2RoleforSSM'
   ssm_role.attach_policy(PolicyArn=ssm_arn)
   atexit.register(ssm_role.detach_policy, PolicyArn=ssm_arn)

Again, we have installed an atexit handler to undo the
association. Without that we would not be able to remove the role
itself.

Now we create an `instance profile` that we will use to launch
instances using our new role.

.. testcode:: -fedex

  instance_profile = iam.create_instance_profile(
    InstanceProfileName=PROFILE_NAME
  )
  atexit.register(instance_profile.delete)

  instance_profile.add_role(RoleName=ssm_role.name)
  atexit.register(instance_profile.remove_role,
                  RoleName=ssm_role.name)

Now it is time to create the virtual network infrastructure for our
system.

.. testcode:: -fedex

   vpc = ec2.create_vpc(CidrBlock=VPC_CIDR)
   atexit.register(vpc.delete)

   public_subnet = vpc.create_subnet(CidrBlock=PUBLIC_CIDR)
   atexit.register(public_subnet.delete)

   private_subnet = vpc.create_subnet(CidrBlock=PRIVATE_CIDR)
   atexit.register(private_subnet.delete)

We have two subnets - one for things that should be accessible from
the Internet and one for all other machines. To make the public subnet
work we need to add an `Internet gateway`_ to it.

.. testcode:: -fedex

   internet_gateway = ec2.create_internet_gateway()
   atexit.register(internet_gateway.delete)

   internet_gateway.attach_to_vpc(VpcId=vpc.vpc_id)
   atexit.register(internet_gateway.detach_from_vpc,
                   VpcId=vpc.vpc_id)

As usual, we have to install ``atexit`` handlers to undo all
operations. This is important not only for operations that create
resources, but also for those that add associations, since AWS will
not allow us to remove a resource with existing associations.

Now we need to define a `route table`_ for the public subnet. The route
table will basically tell our instances to use the Internet gateway
that we have just created to communicate with the Internet.

.. testcode:: -fedex

   public_route_table = vpc.create_route_table()
   atexit.register(public_route_table.delete)

   public_route_table.create_route(
       DestinationCidrBlock='0.0.0.0/0',
       GatewayId=internet_gateway.internet_gateway_id)

   public_rt_assoc = public_route_table.associate_with_subnet(
       SubnetId=public_subnet.id)
   atexit.register(public_rt_assoc.delete)

Machines in the private subnet should not be accessible from the
Internet, but must still be able to access external resources. To
facilitate that we will create a `NAT gateway`_ in the private subnet.
A NAT gateway must have a public IP address, so will get one first.

.. testcode:: -fedex

   nat_eip = ec2_client.allocate_address(Domain='vpc')
   atexit.register(ec2_client.release_address,
                   AllocationId=nat_eip['AllocationId'])

Before we actually create the gateway, we must ensure that we will be
able to decommission it in a safe manner. The issue is that deleting a
NAT gateway is not instantaneous and we cannot remove other resources
(the VPC and subnet) until it is gone. So we will define a function
that periodically checks the status of our gateway and blocks until it
has been fully removed.

.. testcode:: -fedex

   def wait_for_nat_gateway_termination(nat):
       for repeat in range(40):
           time.sleep(10)
           response = ec2_client.describe_nat_gateways(
               NatGatewayIds=[nat['NatGateway']['NatGatewayId']])
           if not response.get('NatGateways', False):
               return
           if response['NatGateways'][0]['State'] in ('deleted', 'failed'):
               return
       raise Exception('NAT gateway refuses to go away')

``boto3`` uses so called ``waiters`` to automate the process of wating
for a state change of an AWS resource. Unfortunately there is no
``waiter`` that checks for the termination of a NAT gateway, so we had
to write our own. We will use ``boto3`` waiters in other parts of the
code.

We are now ready to create the gateway. Notce that the gateway itself
must be a part of the public subent, even though it is meant to be
used by the private subnet.

.. testcode:: -fedex

   nat = ec2_client.create_nat_gateway(
       AllocationId=nat_eip['AllocationId'],
       SubnetId=public_subnet.id
   )
   atexit.register(wait_for_nat_gateway_termination, nat)
   atexit.register(ec2_client.delete_nat_gateway,
                   NatGatewayId=nat['NatGateway']['NatGatewayId'])

Notice two ``atexit`` handlers - one issues the delete command, the
other one waits for its completion. The handlers are executed in
reverse registration order, so the 'waiting' handler is registered
first. We will see this pattern again in the future.

Now we will wait until the NAT gateway is functional This might take a
while. Fortunately this time we can take advantage of a ``boto3``
waiter:

.. testcode:: -fedex

   ec2_client.get_waiter('nat_gateway_available').wait(
       NatGatewayIds=[nat['NatGateway']['NatGatewayId']])

Now we need a route table for the private subnet

.. testcode:: -fedex
   
   private_route_table = vpc.create_route_table()
   atexit.register(private_route_table.delete)

   private_route_table.create_route(
       DestinationCidrBlock='0.0.0.0/0',
       NatGatewayId=nat['NatGateway']['NatGatewayId'])

   private_rt_assoc = private_route_table.associate_with_subnet(
       SubnetId=private_subnet.id)
   atexit.register(private_rt_assoc.delete)

The next pair of resources to create will be the `security
groups`_. These determine what is allowed to connect to what over the
network. We will want different rules for private and public subnets.

.. testcode:: -fedex

   public_security_group = vpc.create_security_group(
       GroupName="ag-http-global",
       Description="Allow SSH + HTTP connections to AG")
   atexit.register(public_security_group.delete)

   public_ip_permissions = [{
       'IpProtocol': 'TCP',
       'FromPort': 10035,
       'ToPort': 10035,
       'IpRanges': [{'CidrIp': MY_IP}]
   }, {
       'IpProtocol': 'TCP',
       'FromPort': 16000,
       'ToPort': 17000,
       'IpRanges': [{'CidrIp': MY_IP}]
   }, {
       'IpProtocol': 'TCP',
       'FromPort': 22,
       'ToPort': 22,
       'IpRanges': [{'CidrIp': MY_IP}]
   }]

   public_security_group.authorize_ingress(
       IpPermissions=public_ip_permissions)

We allow SSH connections and HTTP connections to AG (both the frontend
(``10035``) and the session ports (``16000-17000``)) from the address
we defined above. Note that if you have not changed the default value
of ``MY_IP`` the the system will be accessible from anywhere, which is
a security risk.

Private instances shall accept *all* connections, but only from
machines within our virtual network.

.. testcode:: -fedex

   private_security_group = vpc.create_security_group(
       GroupName="all-internal",
       Description="Allow all access from our network")
   atexit.register(private_security_group.delete)

   private_ip_permissions = [{
       'IpProtocol': '-1',
       'UserIdGroupPairs': [{'GroupId': private_security_group.id},
                            {'GroupId': public_security_group.id}]
   }]

   private_security_group.authorize_ingress(
       IpPermissions=private_ip_permissions)

Now it is time to generate a `keypair`_. This is simply the SSH key
that we will use to control access to our machines.

.. testcode:: -fedex

   key_pair = ec2.create_key_pair(KeyName=KEY_NAME)
   atexit.register(key_pair.delete)

As mentioned above, we will want to use `SSM`_ to execute scripts on
our servers. To do that we have to ensure that an SSM agent is
installed on each machine. We can do this with the following script.

.. testcode:: -fedex

   user_data = """#!/bin/bash
       cd /tmp
       sudo yum install -y https://s3.amazonaws.com/ec2-downloads-windows/SSMAgent/latest/linux_amd64/amazon-ssm-agent.rpm
       sudo start amazon-ssm-agent"""

The script itself will be executed with `cloud-init`_.

Now it is finally time to create our machines. Let us start with the
management node.

.. testcode:: -fedex

   public_instance = ec2.create_instances(
       ImageId=AMI, InstanceType=TYPE,
       UserData=user_data,
       MinCount=1, MaxCount=1,
       KeyName=KEY_NAME,
       SecurityGroupIds=[
           public_security_group.id,
       ],
       IamInstanceProfile={
           "Arn": instance_profile.arn
       },
       SubnetId=public_subnet.id)[0]
   atexit.register(
       ec2_client.get_waiter('instance_terminated').wait,
       InstanceIds=[public_instance.id])
   atexit.register(public_instance.terminate)

We pass our SSM installation script in the ``UserData``
parameter. Notice how the ``atexit`` handlers wait for the instance to
be fully deleted before proceeding to shutdown other systems.

Instances in the private subnet can be created in a similar way.

.. testcode:: -fedex

   private_instances = ec2.create_instances(
       ImageId=AMI, InstanceType=TYPE,
       MinCount=INSTANCE_COUNT, MaxCount=INSTANCE_COUNT,
       UserData=user_data,
       KeyName=KEY_NAME,
       SecurityGroupIds=[
           private_security_group.id,
       ],
       IamInstanceProfile={
           "Arn": instance_profile.arn
       },
       SubnetId=private_subnet.id)
   private_ids = [i.id for i in private_instances]
   all_ids = [public_instance.id] + private_ids
   atexit.register(
       ec2_client.get_waiter('instance_terminated').wait,
       InstanceIds=private_ids)
   for instance in private_instances:
       atexit.register(instance.terminate)

We gather the ids of all our instances in two lists. Now let us wait
until all our instances are operational.

.. testcode:: -fedex

   ec2_client.get_waiter('instance_status_ok').wait(
       InstanceIds=all_ids)

We will need one more public IP address, to be used by the management
instance that we need to connect to.

.. testcode:: -fedex

   eip = ec2_client.allocate_address(Domain='vpc')
   atexit.register(ec2_client.release_address,
                   AllocationId=eip['AllocationId'])
                   
   eip_assoc = ec2_client.associate_address(
       AllocationId=eip['AllocationId'],
       InstanceId=public_instance.id)

   atexit.register(ec2_client.disassociate_address,
                   AssociationId=eip_assoc['AssociationId'])

We have installed the SSM agent on our machines, but it takes a moment
for it to become operational. We will define yet another wait function
to suspend the execution until then.

.. testcode:: -fedex

    def wait_for_ssm_agents(instance_ids):
       for repeat in range(40):
           time.sleep(10)
           response = ssm.describe_instance_information(
               InstanceInformationFilterList=[{
                   'key': 'InstanceIds',
                   'valueSet': instance_ids
               }])
           num_responses = len(response.get(
               'InstanceInformationList', []))
           if num_responses != len(instance_ids):
               # It is normal for an instance to not show up
               # even if requested explicitly.
               continue
           for instance in response.get('InstanceInformationList'):
               if instance['PingStatus'] != 'Online':
                   break
           else:
               return
       raise Exception('Timed out waiting for SSM agents to activate.')  

    wait_for_ssm_agents(all_ids)
    
We will need one more function to wait for an SSM command to complete
its execution (unfortunately there is no built-in waiter for this in
``boto3``).

.. testcode:: -fedex

   def wait_for_ssm_command(command):
       for repeat in range(40):
           time.sleep(10)
           response = ssm.list_commands(
               CommandId=command['Command']['CommandId'])
           if not response.get('Commands', False):
               return
           if response['Commands'][0]['Status'] in \
                   ('Success', 'Cancelled', 'Failed', 'TimedOut'):
               return
       raise Exception(
           'Timed out waiting for an SSM command to finish.')  

Before we proceed, we should wait until all AG servers have started
(at this point we know that the machines are up, but it takes a moment
for AG itself to start processing requests). In a production system we
would probably achieve this by using a dedicated monitoring solution
or some other advanced mechanism, but to keep this tutorial simple we
will just use SSM commands to poll the AG port until it starts
responding.

.. testcode:: -fedex

   wait_for_ag = [
     'until curl -f http://127.0.0.1:10035/version; do sleep 2; done'
   ]
   cmd = ssm.send_command(
       InstanceIds=all_ids,
       DocumentName='AWS-RunShellScript',
       Parameters={'commands': wait_for_ag},
       TimeoutSeconds=120)
   wait_for_ssm_command(cmd)
   
We will now use `SSM`_ to send a script that will load sample data
into our instances. To keep things simple we will add just a single
triple per instance. That triple will include the index of the
instance (a number between ``0`` and ``INSTANCE_COUNT - 1``).

.. testcode:: -fedex

   script = [
       'ID=$(curl http://169.254.169.254/latest/meta-data/ami-launch-index)',
       'curl -X PUT -u test:xyzzy http://127.0.0.1:10035/repositories/r',
       ' '.join(['curl -X PUT -u test:xyzzy http://127.0.0.1:10035/repositories/r/statement',
                 '--data-urlencode "subj=<ex://instance${ID}>"',
                 '--data-urlencode "pred=<ex://id>"',
                 '--data-urlencode "obj=\\"${ID}\\"^^<http://www.w3.org/2001/XMLSchema#integer>"'])
   ]

   cmd = ssm.send_command(InstanceIds=private_ids,
                          DocumentName='AWS-RunShellScript',
                          Parameters={'commands': script})
   wait_for_ssm_command(cmd)

Note that the waiter functions defined above are not particularly
robust in their error handling. In a production system consisting of a
larger number of instances a more elaborate mechanism for error
detection and handling (retries) would have to be implemented.
   
Now we are ready to create the federation by connecting to the
management machine. Notice that we use the default credentials for the
AllegroGraph AMI.
                          
.. testcode:: -fedex

   from franz.openrdf.sail.allegrographserver import AllegroGraphServer

   server = AllegroGraphServer(host=eip['PublicIp'],
                               user='test', password='xyzzy')
   conn = server.openSession(
       '+'.join('<http://test:xyzzy@%s/repositories/r>'
                    % i.private_ip_address
                for i in private_instances))

And now we can issue a query:

.. testcode:: -fedex

   from franz.openrdf.query.query import QueryLanguage
              
   query = conn.prepareTupleQuery(QueryLanguage.SPARQL, """
       SELECT (AVG(?o) as ?avg) { ?s ?p ?o }""")
   query.evaluate(output=True)

This should print the average instance id, which should equal
``(INSTANCE_COUNT - 1) / 2``.

.. testoutput:: -fedex

   -------
   | avg |
   =======
   | 0.5 |
   -------

That is all - the script is now done and will start tearing down the
whole infrastructure.

.. warning:: Remember to check your AWS account for any leftover
             resources after running the tutorial to avoid unnecessary
             costs.

.. |here: ag-ami| replace:: `here <https://franz.com/agraph/ec2/>`__
.. |here: instance-types| replace:: `here <https://aws.amazon.com/ec2/instance-types/>`__
.. |multiple ways: credentials| replace:: `multiple ways <https://boto3.readthedocs.io/en/stable/guide/configuration.html#credentials>`__

.. _AWS: https://aws.amazon.com/
.. _boto3: https://boto3.readthedocs.io/en/stable/
.. _region: http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html
.. _virtual network: https://aws.amazon.com/vpc/
.. _keypair: http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html
.. _security role: http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/iam-roles-for-amazon-ec2.html
.. _SSM: http://docs.aws.amazon.com/systems-manager/latest/userguide/
.. _instance profile: http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_switch-role-ec2_instance-profiles.html
.. _Internet gateway: http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Internet_Gateway.html
.. _NAT gateway: http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-nat-gateway.html
.. _security groups: http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html
.. _route table: http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html
.. _cloud-init: http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html
