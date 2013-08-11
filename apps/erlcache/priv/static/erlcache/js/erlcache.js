function log(m) {
    var date = new Date();
    var time = 
        pad(date.getHours(), 2) + ':' +
        pad(date.getMinutes(), 2) + ':' +
        pad(date.getSeconds(), 2) + '.' +
        pad(date.getMilliseconds(), 3);
    $('#console').append('[' + time + '] ' + m);
    $('#console').append($("<br>"));
    $('#console').scrollTop($('#console').scrollTop()+10000);
}
function pad(a,b){
    return(1e15 + a + "").slice(-b);
}

$(function() {
    log('Erlcache ready');

    var onSuccess = function(result) {
        log('Success: ' + JSON.stringify(result));
    };

    var onError = function(error) {
        log('Error');
        console.error(error);
    }

    $('#postform').submit(function() {
        log('POST action confirmed');
        $.ajax('/action', {
            'type': 'POST',
            'accepts': {
                'json': 'application/json'
            },
            'data': {
                key: $('#postform input#key').val(),
                value: $('#postform input#value').val()
            },
            'success': onSuccess,
            'error': onError
        });
        return false;
    });
    $('#getform').submit(function() {
        log('GET action confirmed');
        $.ajax('/action', {
            'type': 'GET',
            'accepts': {
                'json': 'application/json'
            },
            'data': {
                key: $('#getform input#key').val()
            },
            'success': onSuccess,
            'error': onError
        });
        return false;
    });
});
