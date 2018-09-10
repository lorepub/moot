$( document ).ready(function() {
  new autoComplete({
      selector: '#abstract-author',
      source: function(term, response){
        $.getJSON('/user/search/' + encodeURI(term),
                  {},
                  function(data){
                    console.log(data);
                    response(data);
                  });
      }
  });
});
