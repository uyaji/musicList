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
      $('#artistseqinput').empty();
      $('#artistselect').empty();
      $label = $('<label for="artiselect">artist seq&nbsp&nbsp&nbsp&nbsp</label>');
      $select = $('<select name="artistseq" id="artistseq" style="width:60px;">');
      $('#artistselect').append($label);
      $('#artistselect').append($select);
      datas.forEach ( function( data) {
        $option =$('<option width="3">').val(data).text(data);
        $('#artistseq').append($option);
      });
    }
  });
});
