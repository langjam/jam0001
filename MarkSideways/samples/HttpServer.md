# HTTP Server Example

> As with all MarkSideways programs, this is *NOT* just a markdown file that refers to code. The markdown file itself *IS* the code that MarkSideways interprets. You can run this program by running `python ms.py samples/HttpServer.md [port number]`

This is a server that serves some HTML and JavaScript as the frontend (via GET requests) and has an API endpoint that handles JSON POST requests. 

To start, we determine the port number. If no port is specified as a command line argument, then we use `8080`.

```
if (args.length == 1) {
    portNum = parseInt(args[0]) ?? 8080;
} else if (args.length == 0) {
    portNum = 8080;
} else if (args.length > 0) {
    assert(false, "Incorrect number of arguments! Usage: marksideways HttpServer.md [port]");
}
```

Register handlers for all the static content.

TODO: it'd be nice to have a favicon loaded from disk. This will require file I/O features in the built-in library.

```
files = getStaticFiles();
file_names = files.keys();
for i = 0 till file_names.length {
    name = file_names[i];
    content = files[file_names[i]];
    contentType = name == 'index.html' ? 'text/html' : 'text/javascript';
    http_server_create_handler('GET', '/' + name, SimpleStringResponseHandler.init(contentType, content).handle);
}
```

Add a method to handle the /api POST requests. This doesn't do anything right now other than throw a 500.
```
http_server_create_handler('POST', '/api', apiHandler);
```

Now that the handlers are registered, start the server!
```
http_server_start(portNum);
```

### Get Static Files

```
output = {};
```

This is the main HTML file. Currently it's just boilerplate and loads a `script.js` file.

```
output["index.html"] = "
<!DOCTYPE html>
<html>
    <head>
        <title>Sample HTTP Server with JSON API</title>
        <script src=\"/script.js\"></script>
    </head>
    <body onload=\"main()\">
        Something goes here.
    </body>
</html>
";
```

This is the JavaScript file. It's just a window alert right now.

```
output["script.js"] = "
function main() {
    window.alert('Well, this seems to be running');
}
";
```

The files are returned as a dictionary:

```
return output;
```

### Api Handler

- `method` - the HTTP method of the request
- `content` - the content of the request, if present

```
return {
    'status': 'Internal Server Error',
    'statusCode': 500,
    'contentType': 'text/plain',
    'content': "This handler isn't fully implemetned yet. Please be patient.",
};
```

## Simple String Response Handler

- `contentType` - the content type this handler should return
- `content` - the string value this handler will return

```
this.contentType = contentType;
this.content = content;
```

### Handle

- `method` - the HTTP method of the requset
- `content` - the content of the request, if present

You must return a dictionary with the following keys: `status`, `statusCode`, `contentType`, `content`

```
return {
    'status': 'OK',
    'statusCode': 200,
    'contentType': this.contentType,
    'content': this.content,
};
```

