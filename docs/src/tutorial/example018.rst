.. _example18:

Example 18: Pandas support
--------------------------

The SPARQL query language has somewhat limited capabilities when it
comes to advanced numerical data analysis, data mining and other
similar tasks. In these cases it is best to only use SPARQL to
extract, filter and normalize data (perhaps coming from diverse
sources - the ability to work with such data is one of the key
advantages of the RDF data model) and rely on other tools to perform
further analysis. One of the more popular tools that can be used in
this context is the Pandas_ framework. The AllegroGraph Python client
contains basic support for processing query results with this
library. Let us see how this support can be leveraged in a simplified
scenario.

As usual, we will start by opening a connection.

.. literalinclude:: doctest_setup.py
   :language: python_rdf
   :start-after: BEGIN-CONNECT
   :end-before: END-CONNECT

Now we will add some data. The first data set describes per capita
cheese consumption in the United States in years 2000-2009 according to
USDA. The values are expressed in pounds:
                
.. testcode:: example18

   conn.addData('''
       prefix ex: <ex://>
       ex:c2000 ex:year 2000; ex:cheese 29.8 .
       ex:c2001 ex:year 2001; ex:cheese 30.1 .
       ex:c2002 ex:year 2002; ex:cheese 30.5 .
       ex:c2003 ex:year 2003; ex:cheese 30.6 .
       ex:c2004 ex:year 2004; ex:cheese 31.3 .
       ex:c2005 ex:year 2005; ex:cheese 31.7 .
       ex:c2006 ex:year 2006; ex:cheese 32.6 .
       ex:c2007 ex:year 2007; ex:cheese 33.1 .
       ex:c2008 ex:year 2008; ex:cheese 32.7 .
       ex:c2009 ex:year 2009; ex:cheese 32.8 .
   ''')

Our second set of samples is derived from NSF data and describes the
number of civil engineering doctorates awarded each year.
   
.. testcode:: example18

   conn.addData('''
       prefix ex: <ex://>
       ex:d2000 ex:year 2000; ex:doctorates 480 .
       ex:d2001 ex:year 2001; ex:doctorates 501 .
       ex:d2002 ex:year 2002; ex:doctorates 540 .
       ex:d2003 ex:year 2003; ex:doctorates 552 .
       ex:d2004 ex:year 2004; ex:doctorates 547 .
       ex:d2005 ex:year 2005; ex:doctorates 622 .
       ex:d2006 ex:year 2006; ex:doctorates 655 .
       ex:d2007 ex:year 2007; ex:doctorates 701 .
       ex:d2008 ex:year 2008; ex:doctorates 712 .
       ex:d2009 ex:year 2009; ex:doctorates 708 .
   ''')

We can use a SPARQL query to extract all this data and create a Pandas
DataFrame from it:
   
.. testcode:: example18

   query = '''
   prefix ex: <ex://>   
   select ?year ?cheese ?doctorates {
       _:b1 ex:year ?year ; ex:cheese ?cheese .
       _:b2 ex:year ?year ; ex:doctorates ?doctorates .
   }'''
   with conn.executeTupleQuery(query) as result:
       df = result.toPandas()
   print(df)

.. testoutput:: example18

       year cheese  doctorates
    0  2000   29.8         480
    1  2001   30.1         501
    2  2002   30.5         540
    3  2003   30.6         552
    4  2004   31.3         547
    5  2005   31.7         622
    6  2006   32.6         655
    7  2007   33.1         701
    8  2008   32.7         712
    9  2009   32.8         708

Notice that the DataFrame can be used after the result has been
discarded, since all required data has been copied.

At this point the :meth:`.toPandas` method does not allow fine-grained
control over types of the returned columns. The ``'cheese'`` column
contains decimal values, but floats would be more convenient for
further computation. Thus we will modify the dataframe and convert the
data:

.. testcode:: example18

   df['cheese'] = df['cheese'].astype(float)

Now that we have the data in a form suitable for Pandas, we can
perform some analysis. To keep this tutorial simple we will just
measure the correlation between the number of civil engineering
doctorates awarded and per capita cheese consumption:

.. testcode:: example18

   correlation = df.corr()['cheese']['doctorates']
   print("Correlation: %.5f" % correlation)

.. testoutput:: example18

   Correlation: 0.97433
   
The interpretation of this result is left as an exercise to the
reader.

.. _Pandas: https://pandas.pydata.org/
