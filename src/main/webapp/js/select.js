$('#artistname').on('autocompleteselect', function(ev, ui) {
  $.ajax({
    url : "select/" + encodeURIComponent(ui.item.id),
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
