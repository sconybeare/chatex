function poll(name){
    setTimeout(function(){
        $.ajax({ url: "/next/" + name, success: function(data){
            //Update your dashboard gauge
            var msgDiv = document.getElementById("msgdiv");
            var old = msgDiv.value;
            msgDiv.value = old + '\n' + data.message;
            console.log( "polling" );

            //Setup the next poll recursively
            poll(name);
        }, dataType: "json", method: 'GET'});
    }, 1000);
}

/*(function poll() {
   setTimeout(function() {
       $.getJSON({ url: "/next/seb", success: function(data) {
           var msgDiv = document.getElementById("msgDiv");
           var old = msgDiv.innerHTML;
           msgDiv.innerHTML = old + '<br/>' + data.message;
           console.log( "polling" );
           poll();
       }});
    }, 30000);
})(); */

function handleKeyPress(e) {
    var keynum = e.keyCode;
    var d;
    var val;
    var msgInput;
//    console.log( e );
    if( keynum === 13 ) {
        msgInput = document.getElementById("msginput");
        val = msgInput.value;
        msgInput.value = "";
        d = {
            type: 'POST',
            url: '/submit/' + val
        }
        console.log(d)
        $.ajax( d );
    }

}

function nameKeyPress(e) {
    var keynum = e.keyCode;
    var d;
    var val;
    var nameInput;
//    console.log( e );
    if( keynum === 13 ) {
        nameInput = document.getElementById("nameinput");
        val = nameInput.value;
        nameInput.value = "";
        d = {
            type: 'POST',
            url: encodeURI( '/add/' + val )
        }
        console.log(d)
        $.ajax( d );
        document.getElementById("namediv").style.display = "none";
        poll(val);
    }
}

function spam(n) {
    var d;
    for( var i = 0; i < n; i++ ) {
        d = {
            type: 'POST',
            url: encodeURI( '/submit/' + i )
        }
        $.ajax( d );
    }
}
