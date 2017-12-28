#!/bin/bash

# Generates GPG options for a key passed as a parameter.
# The passphrase can come from these sources:
#   - AG_GPG_PASSPHRASE env var
#   - A file pointed to by $AG_GPG_PASSPHRASE_FILE
#   - The key might be unencrypted.
#
# If the key is encrypted, but there is no passphrase,
# the script will either:
#
#   - return options that will make gpg-agent prompt
#     for the passphrase
#   - or, if your terminal is unlikely to work with
#     gpg-agent, the script will prompt and put the
#     passphrase in the returned options.

set -e

PYPI_GPG_KEY=$1

# Prompt used when reading the passphrase from stdin:
GPG_PROMPT="Enter GPG passphrase for ${PYPI_GPG_KEY} to sign the package:"

# Check if it is safe to use the curses-based gpg-agent prompt
# Note that the condition is also true if TERM is empty or not defined.
case ${TERM} in
    emacs|dumb) AG_NO_GPG_AGENT=y;;
    *) AG_NO_GPG_AGENT=n;;
esac

# This part is always returned (it specifies the key name)
printf -- "-u %q " "${PYPI_GPG_KEY}"

if [ -z "${AG_GPG_PASSPHRASE+x}" ]; then
    # Read the passphrase from a variable.
    # Note that this is insecure since the passphrase will appear
    # on command line of gpg
    printf -- "--passphrase %q" "${AG_GPG_PASSPHRASE}"
elif [ -z "${AG_GPG_PASSPHRASE_FILE+x}" ]; then
    # Passphrase saved to a file
    printf -- "--passphrase-file %q" "${AG_GPG_PASSPHRASE_FILE}"
elif [ "${AG_NO_GPG_AGENT}" == "y" ]; then
    # We must avoid gpg-agent because the terminal can't support it
    # First check if we really need a passphrase
    if gpg --batch -o /dev/null --passphrase-fd 0 \
           --local-user "${PYPI_GPG_KEY}" -as - 2> /dev/null; then
        # No passphrase needed
        echo -n
    else
        # Prompt manually to avoid gpg-agent.
        # This is as insecure as using AG_GPG_PASSPHRASE
        read -s -r -p '${GPG_PROMPT}' PASS
        printf -- "--passphrase %q" "${PASS}"
    fi
else
    # Just rely on gpg-agent
    echo -n
fi

