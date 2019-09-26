import re, os

def get_password_keychain(service, account):
    command = "/usr/bin/security find-generic-password -g -w -s %s -a %s" % (service, account)
    return os.popen(command).read().rstrip()
