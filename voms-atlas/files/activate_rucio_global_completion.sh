# Authors:
# - Vincent Garonne, <vgaronne@gmail.com>, 2018

shopt -s checkwinsize

# Script to enable shell completion on the rucio commands

eval "$(register-python-argcomplete rucio)"
eval "$(register-python-argcomplete rucio-admin)"
