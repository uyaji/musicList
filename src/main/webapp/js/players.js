$('.players').hover(function () {
  $.ajax({
    url : "selectPlayers/" + $(this).parent().parent().attr('id') + "/" + $('#searchPlayer').val(),
    dataType: "json",
    success: function( datas ) {
      $(".inner").empty();
      datas.forEach( function( data ) {
        $(".inner").append(data + "<br/>");
      });
      var navClass = $(".modalOpen").attr("class"),
      href = $(".modalOpen").attr("href");
            
      $(href).fadeIn();
      $(".modalOpen").addClass("open");
      return false;
    }
  });},
  function () {
    $(".modalClose").parents(".modal").fadeOut();
    $(".modalOpen").removeClass("open");
    return false;
  }
);
