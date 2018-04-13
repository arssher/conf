# -*- coding: utf-8 -*-

from subprocess import check_output
import sys

# to use it,
# sudo apt-get install pass
# pass init ars
# pass insert mail/dummy dummy
# pass insert mail/pgpro
# pass insert mail/yandex
# ... etc

def get_pass(account):
    return check_output("pass mail/{}".format(account),
                        shell=True).strip('\n')

# exit, if passwords can't be read
if (get_pass('dummy') != 'dummy'):
    sys.stderr.write("Error reading in passwords. Terminating.\n")
    getglobalui().terminate()

# for debug
if __name__ == "__main__":
    print(get_pass('pgpro'))


# utf7 imap stuff
# from: http://piao-tech.blogspot.no/2010/03/get-offlineimap-working-with-non-ascii.html#resources

import binascii
import codecs

# encoding

def modified_base64 (s):
  s = s.encode ('utf-16be')
  return binascii.b2a_base64(s).rstrip('\n=').replace('/', ',')

def doB64(_in, r):
  if _in:
    r.append ('&%s-' % modified_base64(''.join(_in)))
    del _in[:]

def encoder(s):
  r = []
  _in = []
  for c in s:
    ordC = ord(c)
    if 0x20 <= ordC <= 0x25 or 0x27 <= ordC <= 0x7e:
      doB64(_in, r)
      r.append (c)
    elif c == '&':
      doB64(_in, r)
      r.append ('&-')
    else:
      _in.append(c)
  doB64(_in, r)
  return (str(''.join(r)), len(s))

# decoding
def modified_unbase64(s):
  b = binascii.a2b_base64(s.replace(',', '/') + '===')
  return unicode (b, 'utf-16be')

def decoder (s):
  r = []
  decode = []
  for c in s:
    if c == '&' and not decode:
      decode.append ('&')
    elif c == '-' and decode:
      if len(decode) == 1:
        r.append('&')
      else:
        r.append(modified_unbase64(''.join(decode[1:])))
      decode = []
    elif decode:
      decode.append(c)
    else:
      r.append(c)

  if decode:
    r.append(modified_unbase64(''.join(decode[1:])))
  bin_str = ''.join(r)
  return (bin_str, len(s))

class StreamReader (codecs.StreamReader):
  def decode (self, s, errors='strict'):
    return decoder(s)

class StreamWriter (codecs.StreamWriter):
  def decode (self, s, errors='strict'):
    return encoder(s)

def imap4_utf_7(name):
  if name == 'imap4-utf-7':
    return (encoder, decoder, StreamReader, StreamWriter)

codecs.register(imap4_utf_7)

# IMAP uses weird imap4-utf-7 encoding, need to translade folder names
# accordingly
nametrans = {u'Спам': u'Spam',
             u'Исходящие': u'Sending',
             u'Удаленные': u'Trash',
             u'Отправленные': u'Sent',
             u'Черновики': u'Drafts'}
inv_nametrans = {v: k for k, v in nametrans.items()}
def imap2utf8(s):
    decoded = s.decode('imap4-utf-7')
    decoded = nametrans.get(decoded, decoded)
    return decoded.encode('utf8')
def utf82imap(s):
    decoded = s.decode('utf8')
    decoded = inv_nametrans.get(decoded, decoded)
    return decoded.encode('imap4-utf-7')
