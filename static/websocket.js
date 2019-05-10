ElmWS = { app : undefined,
          fd : []
        }

//
// Callback generators
//

// Sends websocket information to Elm upon successfully opening a WebSocket
ElmWS.__open__ = function(socket, url, fd)
{
  var gen = function()
  {
    ElmWS.app.ports.newFD.send({ url : url,
                                 fd : fd
                               });
    socket.onopen = function() {;};
  }
  return(gen);
}

// Sends received data to Elm with websocket info upon receipt
ElmWS.__recv__ = function(fd)
{
  var gen = function(message)
  {
    ElmWS.app.ports.recv.send({ fd : fd,
                                data : JSON.parse(message.data),
                                timeStamp : message.timeStamp
                              });
  }
  return(gen);
}

//
// User-invoked functions
//

// createWS : (String, List String) -> Cmd msg
ElmWS.create = function(spec)
{
  var url, protos, socket, fd;

  [url, protos] = spec;
  socket = new WebSocket(url, protos);
  fd     = ElmWS.fd.push(socket) - 1;

  socket.onopen = ElmWS.__open__(socket, url, fd);
  socket.onmessage = ElmWS.__recv__(fd);
}

// send : {socket : { fd : Int }, data : { msg : String }} -> Cmd msg
ElmWS.send = function(spec)
{
  var socket = ElmWS.fd[spec.socket.fd];
  if(socket !== undefined && socket.readyState == 1)
  {
    socket.send(JSON.stringify(spec.data));
  }
}

ElmWS.init = function(app)
{
  ElmWS.app = app;

  app.ports.createWS.subscribe(ElmWS.create);
  app.ports.sendWS.subscribe(ElmWS.send);
}

// Passed in via flag
websocket_uri = "ws://" + window.location.host;
