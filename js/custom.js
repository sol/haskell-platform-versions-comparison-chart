// vim: noai:ts=2:sw=2

$(document).ready(function() {
  loadLatest();
});


// load latest package versions
function loadLatest() {

  // enable activity indicator
  $('th.latest').activity({segments: 8, width: 2, space: 0, length: 3, speed: 1.5, align: 'right'});

  $.getScript("http://www.typeful.net/~tbot/hackage-package-versions.jsonp", function() {

    // disable activity indicator
    $('th.latest').activity(false);

    var response = $(hackagePackageVersions)
    hackagePackageVersions = null;

    $('td.latest').each(function() {

      var self = $(this);
      var version = response.attr(self.data("package"));
      if (version) {
        var c = self.children("a");
        c.text(version);
        if (self.next().data("version") != version) {
          c.addClass("alert-success");
        }
      }
    });
  });
}
