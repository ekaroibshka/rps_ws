

var websocket;
$(document).ready(init);

function init() {
  $('#server').val("ws://" + window.location.host + "/websocket");
  if(!("WebSocket" in window)){  
      $('#status').append('<p><span style="color: red;">websockets are not supported </span></p>');
      $("#navigation").hide();  
  } else {
      $('#status').append('<p><span style="color: green;">websockets are supported </span></p>');
      connect();
  };
      $("#connected").hide(); 	
      $("#content").hide(); 	
};

function connect()
{
  wsHost = $("#server").val()
  websocket = new WebSocket(wsHost);
  showScreen('Connecting to: ' +  wsHost + '\n'); 
  websocket.onopen = function(evt) { onOpen(evt) }; 
  websocket.onclose = function(evt) { onClose(evt) }; 
  websocket.onmessage = function(evt) { onMessage(evt) }; 
  websocket.onerror = function(evt) { onError(evt) }; 
};  

function disconnect() {
  websocket.close();
}; 

function toggle_connection(){
  if(websocket.readyState == websocket.OPEN){
      disconnect();
  } else {
      connect();
  };
};

function sendTxt(event) {

if (event.which == 13 || event.keyCode == 13) {
  if($("#send_txt").val().length === 0) {
    alert('Type something!');
    return true;
  } else {
    if(websocket.readyState == websocket.OPEN){
        txt = $("#send_txt").val();
        $("#send_txt").val('');
        websocket.send( txt );
        showScreen( 'YOU: ' +  txt + '\n'); 
    } else {
         showScreen('websocket is not connected'); 
    };
  };
  return false;
}
return true;
};

function onOpen(evt) { 
  showScreen('CONNECTED\n'); 
  $("#connected").fadeIn('slow');
  $("#content").fadeIn('slow');
};  

function onClose(evt) { 
  showScreen('DISCONNECTED\n');
};  

function onMessage(evt) { 
  showScreen(evt.data + '\n'); 
};  

function onError(evt) {
  showScreen('ERROR: ' + evt.data + '\n');
};

function showScreen(txt) { 
  $('#output').append(txt);
};

function clearScreen() 
{ 
  $('#output').val('');
};