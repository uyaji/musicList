$('.players').hover(function() {
  $.ajax({
    url : "selectPlayers/" + $(this).parent().parent().attr('id') + "/" + $('#searchPlayer').val(),
    dataType: "json",
    success: function( datas ) {
        alert(datas);
    }
  });},
  function(){}
);
