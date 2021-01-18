export default function(app)
{
  var ElmWS = { fd : [] }

  //
  // Callback generators
  //

  // Sends websocket information to Elm upon successfully opening a WebSocket
  ElmWS.__open__ = function(socket, url, fd)
  {
    return(function()
    {
      app.ports.newFD.send({ url : url
                           , fd : fd
                           });
    });
  }

  // Sends received data to Elm with websocket info upon receipt
  ElmWS.__recv__ = function(fd)
  {
    return(function(message)
    {
      app.ports.recv.send({ fd : fd,
                            data : JSON.parse(message.data),
                            timeStamp : message.timeStamp
                          });
    });
  }

  // Forcibly close after error
  ElmWS.__error__ = function(socket, fd)
  {
    return(function(evt)
    {
      app.ports.error.send({ fd : fd });
      socket.close();
    });
  }

  // Reconnect after error/close
  ElmWS.__reconnect__ = function(fd, url, protos)
  {
    return(function()
    {
      var socket;
      socket = new WebSocket(url, protos);

      ElmWS.fd[fd] = socket;

      socket.onopen = ElmWS.__open__(socket, url, fd);
      socket.onmessage = ElmWS.__recv__(fd);
      socket.onerror = ElmWS.__error__(socket, fd);
      socket.onclose = ElmWS.__reconnect__(fd, url, protos);

      app.ports.reopenedFD.send({ url : url
                                , fd : fd
                                });
    });
  }

  //
  // User-invoked functions
  //

  // createWS : (String, List String) -> Cmd msg
  ElmWS.create = function(spec)
  {
    var url, protos, socket, fd, websocket_uri;

    [url, protos] = spec;
    websocket_uri = window.location.protocol.replace(/^http/, 'ws') + '//' + url;

    socket = new WebSocket(websocket_uri, protos);
    fd     = ElmWS.fd.push(socket) - 1;

    socket.onopen    = ElmWS.__open__(socket, websocket_uri, fd);
    socket.onmessage = ElmWS.__recv__(fd);
    socket.onerror   = ElmWS.__error__(socket, fd);
    socket.onclose   = ElmWS.__reconnect__(fd, websocket_uri, protos);
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

  app.ports.createWS.subscribe(ElmWS.create);
  app.ports.sendWS.subscribe(ElmWS.send);

  return(ElmWS);
}
