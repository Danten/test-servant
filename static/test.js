function myFunction() {
    var request = new XMLHttpRequest();
    request.open('POST', "api");
    request.responseType = 'json';
    request.send();

    request.onload = function() {
        // var answer = JSON.parse(request.response);
        alert(request.response);
    }
}
