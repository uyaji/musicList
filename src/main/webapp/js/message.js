$('#message').hover(function () {
  $.ajax({
    url : "selectTwiters/" + $('#user').val(),
    dataType: "json",
    success: function( datas ) {
      $(".inner").empty();
      datas.forEach( function( data ) {
        $(".inner").append("<a href =twit?from=" + $('#user').val() + "&to=" + data.value + ">"+ data.label + "</a><br/>");
      });
      $(".inner").append("<br /><a href='' onClick='$(\".modalClose\").parents(\".modal\").fadeOut();$(\".modalOpen\").removeClass(\"open\");return false;'>exit</a>");
      var navClass = $(".modalOpen").attr("class"),
      href = $(".modalOpen").attr("href");
            
      $(href).fadeIn();
      $(".modalOpen").addClass("open");
      return false;
    }
  });}
,
  function () {
  }
);
