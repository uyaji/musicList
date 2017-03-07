$(function() {
  $('.search').autocomplete( {
    source: function( req, res ) {
      $.ajax({
        url: "suggest/" + encodeURIComponent(req.term) + "/" + this.element.attr("id"),
        dataType: "json",
        success: function( data ) {
          res(data);
        }
      });
    }
    ,autoFocus:true,delay: 500,minLength: 0});
});
$('#artistname').on('autocompleteselect', function(ev, ui) {
  $.ajax({
    url : "select/" + encodeURIComponent(ui.item.label),
    dataType: "json",
    success: function( datas ) {
      $('#artistseq').empty();
      datas.forEach ( function( data) {
        $option =$('<option>').val(data).text(data);
        $('#artistseq').append($option);
      });
    }
  });
});
