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
  ElmWS.__reconnect__ = function(fd, url, config, retries)
  {
    if(!config.autoReconnect || (config.reconnectMaxTries && (retries > config.reconnectMaxTries))) { return(function(){;}); }

    return(function()
    {
      var timeout = Math.pow(config.reconnectWait, retries * config.reconnectBackoffMultiplier);
      if(config.reconnectBackoffMaxWait && config.reconnectBackoffMaxWait < timeout)
      {
        timeout = config.reconnectBackoffMaxWait;
      }

      setTimeout(function()
      {
        var socket = new WebSocket(url, config.protocols);
        ElmWS.fd[fd] = socket;

        socket.onclose = ElmWS.__reconnect__(fd, url, config, retries+1);
        socket.onmessage = ElmWS.__recv__(fd);
        socket.onerror = ElmWS.__error__(socket, fd);

        socket.onopen = function()
        {
          app.ports.reopenedFD.send({ url : url
                                    , fd : fd
                                    });
        }
      }, timeout * 1000);
    });
  }

  //
  // User-invoked functions
  //

  // createWS : (String, SocketConfig) -> Cmd msg
  ElmWS.create = function(spec)
  {
    var url, config, socket, fd, websocket_uri;

    [url, config] = spec;
    websocket_uri = window.location.protocol.replace(/^http/, 'ws') + '//' + url;

    socket = new WebSocket(websocket_uri, config.protocols);
    fd     = ElmWS.fd.push(socket) - 1;

    socket.onopen    = ElmWS.__open__(socket, websocket_uri, fd);
    socket.onclose   = ElmWS.__reconnect__(fd, websocket_uri, config, 0);
    socket.onmessage = ElmWS.__recv__(fd);
    socket.onerror   = ElmWS.__error__(socket, fd);
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
