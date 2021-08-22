from http.server import BaseHTTPRequestHandler, HTTPServer
from urllib.parse import urlparse

import os
import sys
import threading
import time

from .exceptions import *
from .nodes import run_function_value
from .values import *

_handlers = {}
_request_queue = []
_request_queue_mutex = None

class MswHttpHandler(BaseHTTPRequestHandler):
  def do_GET(self):
    self._handle('GET')

  def do_POST(self):
    self._handle('POST')

  def do_HEAD(self):
    self._handle('HEAD')

  def do_PUT(self):
    self._handle('PUT')

  def do_DELETE(self):
    self._handle('DELETE')

  def do_PATCH(self):
    self._handle('PATCH')
  
  def _handle(self, method):
    parts = urlparse(self.path)
    path = parts.path
    query = parts.query
    fragment = parts.fragment

    # I think this is fine to run for all requests since the Content-Length field will be 0
    content_length = int(self.headers.get('content-length', 0))
    content_type = self.headers.get('content-type', '')
    content = None
    if content_length > 0:
      content = self.rfile.read(content_length)

    request = {
      'ready': False,
      'method': method,
      'path': path,
      'query': query, # TODO: convert this into a dictionary
      'fragment': fragment,
      'contentType': content_type,
      'content': content,
    }
    
    _request_queue_mutex.acquire()
    _request_queue.append(request)
    _request_queue_mutex.release()

    waiting = True
    while waiting:
      time.sleep(0.05)
      _request_queue_mutex.acquire()
      if request['ready']:
        waiting = False
      _request_queue_mutex.release()

    self.send_response(request['statusCode'])
    self.send_header('Content-type', request['responseContentType'])
    self.end_headers()
    if method != 'HEAD':
      self.wfile.write(request['response'].encode('utf-8'))

def run_http_handler(throw_token, handler, global_scope, method, content):
  result_value = run_function_value(throw_token, handler, global_scope, [StringValue(method), StringValue(content)])
  
  if not result_value.is_error and result_value.type != 'DICTIONARY': 
    result_value = new_error_value(throw_token, "Handler function must return a dictionary!")

  if result_value.is_error:
    print_stack_trace(result_value.stackTrace)
    content = StringValue('An error has occurred on the server')
    content_type = StringValue('text/plain')
    status_code = get_integer_value(500)
  else:
    content = result_value.get_item(throw_token, StringValue('content'))
    content_type = result_value.get_item(throw_token, StringValue('contentType'))
    status_code = result_value.get_item(throw_token, StringValue('statusCode'))

  if status_code == None:
    status_code = 200
  else:
    status_code = status_code.value if status_code.type == 'INT' else 400

  return {
    'content': '' if content == None else content.to_string(),
    'contentType': 'text/html' if content_type == None else content_type.to_string(),
    'statusCode': status_code,
  }

def poll_and_run_queue(throw_token, scope_globals):
  request = None
  _request_queue_mutex.acquire()
  if len(_request_queue) > 0:
    request = _request_queue.pop(0)
  _request_queue_mutex.release()

  if request == None: return

  url = request['path']
  method = request['method']
  handler = _handlers.get(method + ':' + url)
  if handler == None:
    response = '<html><head><title>Not found!</title><style type="text/css">body { font-family: sans-serif; }</style></head><body><h1>Mark Sideways: 404</h1><p>It looks like there is nothing at that URL!</p></body></html>'
    responseContentType = 'text/html'
    statusCode = 404
    status = 'Not Found'
  else:
    # TODO: the handler should accept headers, content-type, the path, query, fragment, etc.
    output = run_http_handler(throw_token, handler, scope_globals, method, request['content'])

    response = output.get('content', '')
    responseContentType = output.get('contentType', 'text/html')
    statusCode = output.get('statusCode', 200)
    status = 'OK'
  
  _request_queue_mutex.acquire()
  request['statusMessage'] = status
  request['statusCode'] = statusCode
  request['response'] = response
  request['responseContentType'] = responseContentType
  request['ready'] = True
  _request_queue_mutex.release()

def get_httpserve_lib():

  def _create_handler(throw_token, method, path, handler_value):
    _handlers[method + ":" + path] = handler_value
    return NULL_VALUE

  def _start(throw_token, port, scope_globals):
    global _request_queue_mutex
    _request_queue_mutex = threading.Lock()

    server = HTTPServer(('', port), MswHttpHandler)
    
    thread = threading.Thread(target = server.serve_forever)
    thread.daemon = True
    thread.start()

    try:
      while True:
        time.sleep(0.05)
        poll_and_run_queue(throw_token, scope_globals)
        
    except KeyboardInterrupt:
      pass
    return new_error_value(throw_token, "Server stopped by keyboard")

  return {
    'create_handler': _create_handler,
    'start': _start,
  }
