// mycode.js

$(document).ready(function() {
  getLocation();
  

  
  function showLocation(position) {
    var latitude = position.coords.latitude;
    var longitude = position.coords.longitude;
    Shiny.onInputChange("lat", latitude);
    Shiny.onInputChange("lon", longitude);
      }
  
   
      function errorHandler(err) {
    switch(err.code) {
      case err.PERMISSION_DENIED:
        alert( "User denied the request for Geolocation.")  ;
      break;
      case err.POSITION_UNAVAILABLE:
        alert("Location information is unavailable.");
      break;
      case err.TIMEOUT:
        alret("The request to get user location timed out.");
      break;
      case err.UNKNOWN_ERROR:
        alret("An unknown error occurred.");
      break;
    }}
  
  
    function getLocation(){
    
    if(navigator.geolocation){
      // timeout at 60000 milliseconds (60 seconds)
      var options = {timeout:60000};
      navigator.geolocation.getCurrentPosition(showLocation, errorHandler, options);
    }
    
    else{
      alert("Sorry, browser does not support geolocation!");
    }
  }
  
});