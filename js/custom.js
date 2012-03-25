// vim: noai:ts=2:sw=2

$(document).ready(function() {
  loadLatest();
});


// load latest package versions
function loadLatest() {

  // enable activity indicator
  $('.hackage-header').activity({segments: 8, width: 2, space: 0, length: 3, speed: 1.5, align: 'right'});

  $.getJSON("http://www.typeful.net/~tbot/log.json", function(data) {

    // disable activity indicator
    $('.hackage-header').activity(false);

    $('.hackage-version').each(function() {

      var self = $(this);
      var version = $(data).attr(self.data("package"));
      if (version) {
        if (self.next().data("version") != version) {
          self.html('<span class="alert-success">' + version + '</span>');
        } else {
          self.text(version);
        }
      }

    });
  });
}
