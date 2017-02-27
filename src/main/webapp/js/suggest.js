$(function() {
  $('.search').autocomplete( {
    source: function( req, res ) {
      $.ajax({
        url: "suggest/" + encodeURIComponent(req.term) + "/" + this.element.attr("name"),
        dataType: "json",
        success: function( data ) {
          res(data);
        }
      });
    }
    ,autoFocus:true,delay: 500,minLength: 0});
});
