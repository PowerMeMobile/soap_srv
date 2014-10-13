# -*- coding: utf-8 -*-

# setup python's virtualenv as described here
# https://gist.github.com/ten0s/98e7d88476ec75351d75

# $ . env/bin/activate
# $ py.test soap_test.py
# $ py.test soap_test.py -k test_encodings
# $ py.test --pdb
# $ py.test -v

# make standalone test script and then run it in verbose mode
# $ py.test --genscript=runtests.py
# $ python runtests.py -v

import pytest
import hexdump

SOAP11 = 'soapenv'

WSDL = 'http://localhost:8088/bmsgw/soap/messenger.asmx?WSDL'
#WSDL = 'http://mm.powermemobile.com/mm/soap/messenger.asmx?WSDL'

CUSTOMER_ID = 3
USER_ID     = 'user'
PASSWORD    = 'password'
BAD_PASSWORD = 'intentionally wrong password'

USER = {
    'CustomerID':CUSTOMER_ID,
    'Name':USER_ID,
    'Language':'en',
    'Password':PASSWORD
}

ORIGINATOR = 'SMS'
RECIPIENT = '375293615363'

@pytest.fixture(scope="module")
def client(request):
    from pysimplesoap.client import SoapClient

    wsdl = WSDL
    soap_ns = SOAP11

    client = SoapClient(wsdl=wsdl, soap_ns=soap_ns, trace=False)

    # Statistics and Details parsers
    # workaround of <s:sequence><s:any/></s:sequence>

    # SMSC_DELIVERED, 3e231a28-cdf2-4d95-ac2e-a8cd1e0a0a0a - TempFail_WillRetry
    statistics = {'SMSC_PENDING':int, 'SMSC_SUBMITTED':int, 'SMSC_FAILED':int, 'SMSC_DELIVERED':int, 'SMSC_EXPIRED':int, 'SMSC_DELETED':int, 'SMSC_UNDELIVERED':int, 'SMSC_ACCEPTED':int, 'SMSC_UNKNOWN':int, 'SMS_REJECTED':int}
    client.services['Messenger']['ports']['MessengerSoap']['operations']['GetSmsStatus']['output']['GetSmsStatusResponse']['GetSmsStatusResult']['Statistics'] = {'statistics':statistics}

    return client

#
# SendSms
#

# messageType=Latin|ArabicWithArabicNumbers|ArabicWithLatinNumbers

def check_message_parts_count(client, message, encoding, count):
    res = client.SendSms(user=USER, originator=ORIGINATOR, smsText=message, recipientPhone=RECIPIENT, messageType=encoding, defDate='', blink=False, flash=False, Private=False)
    assert res['SendSmsResult']['Result'] == 'OK'
    assert res['SendSmsResult']['TransactionID'] != None
    tid = res['SendSmsResult']['TransactionID']

    res = client.GetSmsStatus(user=USER, transactionID=tid, detailed=False)
    assert res['GetSmsStatusResult']['Result'] == 'OK'
    print res
    assert sum(res['GetSmsStatusResult']['Statistics']['statistics'].values()) == count

def test_check_encodings(client):
    latin1_160 = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJ'
    latin1_161 = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJK'
    latin1_306 = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ012345'
    latin1_307 = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456'
    latin1_459 = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxy'
    latin1_460 = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz'
    checks = [
        (latin1_160, 'Latin', 1),
        (latin1_161, 'Latin', 2),
        (latin1_306, 'Latin', 2),
        (latin1_307, 'Latin', 3),
        (latin1_459, 'Latin', 3),
        (latin1_460, 'Latin', 4)
        ## utf-8 encoding tests are coming
    ]

    for (message, encoding, count) in checks:
        check_message_parts_count(client, message, encoding, count)
