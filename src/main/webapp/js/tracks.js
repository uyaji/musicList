$('.tracktitles').hover(function() {
  $.ajax({
    url : "selectTracks/" + $(this).parent().parent().attr('id') + "/" + $('#searchTrack').val(),
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
  function(){
    $(".modalClose").parents(".modal").fadeOut();
    $(".modalOpen").removeClass("open");
    return false;
  }
);
