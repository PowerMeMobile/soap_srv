#!/usr/bin/env python

#git clone https://github.com/pysimplesoap/pysimplesoap.git
#cd pysimplesoap
#sudo ./setup.py install
#python

# https://code.google.com/p/pysimplesoap/wiki/SoapClient

origPhone='999'

# don't use 'soap12' it's wrong
soap_ns='soapenv' # 'soapenv' | 'soap12env'

from pysimplesoap.client import SoapClient
client = SoapClient(wsdl="http://localhost:8088/bmsgw/soap/messenger.asmx?wsdl", soap_ns=soap_ns, trace=False)

# fix content type for soap12
if soap_ns == 'soap12env':
    client.http_headers = {'Content-type':'application/soap+xml;charset=utf-8'}

# return Statistics and Details as embedded XML
#client.services['Messenger']['ports']['MessengerSoap']['operations']['GetSmsStatus']['output']['GetSmsStatusResponse']['GetSmsStatusResult']['Statistics'] = None
#client.services['Messenger']['ports']['MessengerSoap']['operations']['GetSmsStatus']['output']['GetSmsStatusResponse']['GetSmsStatusResult']['Details'] = None

# parse Statistics and Details
statistics = {'submitted':int, 'sent':int, 'failed':int, 'delivered':int, 'expired':int, 'deleted':int, 'undeliverable':int, 'accepted':int, 'unknown':int, 'rejected':int}
client.services['Messenger']['ports']['MessengerSoap']['operations']['GetSmsStatus']['output']['GetSmsStatusResponse']['GetSmsStatusResult']['Statistics'] = {'statistics':statistics}

numbers = [{'number':str, 'TimeStamp':str}]
details = {'submitted':numbers, 'sent':numbers, 'failed':numbers, 'delivered':numbers, 'expired':numbers, 'deleted':numbers, 'undeliverable':numbers, 'accepted':numbers, 'unknown':numbers, 'rejected':numbers}
client.services['Messenger']['ports']['MessengerSoap']['operations']['GetSmsStatus']['output']['GetSmsStatusResponse']['GetSmsStatusResult']['Details'] = {'details':details}

# calls
client.KeepAlive(user={'CustomerID':'3', 'Name':'user', 'Language':'en', 'Password':'password'})
#{'KeepAliveResult': {'Result': u'OK'}}

client.Authenticate(user={'CustomerID':'3', 'Name':'user', 'Language':'en', 'Password':'password'})
#{'AuthenticateResult': {'CreditMMS': None, 'Originators': [{'string': u'999'}], 'NetPoints': u'POSTPAID', 'Result': u'OK', 'CreditSMS': u'POSTPAID', 'CustomerID': 3}}

client.SendSms(user={'CustomerID':'3', 'Name':'user', 'Language':'en', 'Password':'password'}, originator=origPhone, smsText='Hello', recipientPhone='375296543210', messageType='Latin')
#{'SendSmsResult': {'NetPoints': u'POSTPAID', 'TransactionID': u'915c1f0e-0ce8-11e4-9d4c-00269e42f7a5', 'RejectedNumbers': [], 'Result': u'OK'}}

client.GetSmsStatus(user={'CustomerID':'3', 'Name':'user', 'Language':'en', 'Password':'password'}, transactionID='915c1f0e-0ce8-11e4-9d4c-00269e42f7a5', detailed=False)
#{'GetSmsStatusResult': {'NetPoints': u'POSTPAID', 'Statistics': {'statistics': {'delivered': 1}}, 'Result': u'OK', 'Details': None}}

client.GetSmsStatus(user={'CustomerID':'3', 'Name':'user', 'Language':'en', 'Password':'password'}, transactionID='915c1f0e-0ce8-11e4-9d4c-00269e42f7a5', detailed=True)
#{'GetSmsStatusResult': {'NetPoints': u'POSTPAID', 'Statistics': {'statistics': {'delivered': 1}}, 'Result': u'OK', 'Details': {'details': {'delivered': [{'number': u'375296543210'}]}}}}

client.SendBinarySms(user={'CustomerID':'3', 'Name':'user', 'Language':'en', 'Password':'password'}, originator=origPhone, binaryBody='7465737420746573742074657374207465737420', recipientPhone='375296543210', data_coding=4, esm_class='')
#{'SendBinarySmsResult': {'NetPoints': u'POSTPAID', 'TransactionID': u'ed1a3708-0d93-11e4-9d4c-00269e42f7a5', 'RejectedNumbers': [], 'Result': u'OK'}}

client.SendServiceSms(customerID='3', userName='user', userPassword='password', originator=origPhone, serviceName='google', serviceUrl='http://google.com', recipientPhone='375296543210', messageType='Latin')
#{'SendServiceSmsResult': {'NetPoints': u'POSTPAID', 'TransactionID': u'946b8480-0d94-11e4-9d4c-00269e42f7a5', 'RejectedNumbers': [], 'Result': u'OK'}}

client.SendSms2(user={'CustomerID':'3', 'Name':'user', 'Language':'en', 'Password':'password'}, originator=origPhone, smsText='Hello', recipientPhonesFile='Mzc1Mjk2NTQzMjEwLDM3NTI5NjU0MzIxMSwzNzUyOTY1NDMyMTI=', messageType='Latin')
#{'SendSms2Result': {'NetPoints': u'POSTPAID', 'TransactionID': u'e2d81868-0d94-11e4-9d4c-00269e42f7a5', 'RejectedNumbers': [], 'Result': u'OK'}}

client.HTTP_KeepAlive(customerID='3', userName='user', userPassword='password')
#{'HTTP_KeepAliveResult': {'Result': u'OK'}}

client.HTTP_Authenticate(customerID='3', userName='user', userPassword='password')
#{'HTTP_AuthenticateResult': {'CreditMMS': None, 'Originators': [{'string': u'999'}], 'NetPoints': u'POSTPAID', 'Result': u'OK', 'CreditSMS': u'POSTPAID', 'CustomerID': 3}}

client.HTTP_SendSms(customerID='3', userName='user', userPassword='password', originator=origPhone, smsText='Hello', recipientPhone='375296543210', messageType='Latin')
#{'HTTP_SendSmsResult': {'NetPoints': u'POSTPAID', 'TransactionID': u'589ddc2a-0d9c-11e4-9d4c-00269e42f7a5', 'RejectedNumbers': [], 'Result': u'OK'}}

client.HTTP_GetSmsStatus(customerID='3', userName='user', userPassword='password', transactionID='915c1f0e-0ce8-11e4-9d4c-00269e42f7a5', detailed=True)
#{'HTTP_GetSmsStatusResult': {'NetPoints': u'POSTPAID', 'Statistics': {'statistics': {'delivered': 1}}, 'Result': u'OK', 'Details': {'details': {'delivered': [{'number': u'375296543210'}]}}}}

client.HTTP_SendBinarySms(customerID='3', userName='user', userPassword='password', originator=origPhone, binaryBody='7465737420746573742074657374207465737420', recipientPhone='375296543210', data_coding=4, esm_class='')
#{'HTTP_SendBinarySmsResult': {'NetPoints': u'POSTPAID', 'TransactionID': u'd5b08bc2-0d9c-11e4-9d4c-00269e42f7a5', 'RejectedNumbers': [], 'Result': u'OK'}}
