// vim: noai:ts=2:sw=2

$(document).ready(function() {
  loadLatest();
});


// load latest package versions
function loadLatest() {

  // enable activity indicator
  $('th.latest').activity({segments: 8, width: 2, space: 0, length: 3, speed: 1.5, align: 'right'});

  $.getScript("http://www.typeful.net/~tbot/hackage/latest-package-versions.jsonp");
}

// this is triggered by loadLatest()
function hackagePackageVersionsCallback(response) {

  var versions = $(response);

  // insert latest version for each package
  $('td.latest').each(function() {
    var self = $(this);
    var version = versions.attr(self.data("package"));
    if (version) {
      var c = self.children("a");
      c.text(version);
      if (self.next().data("version") != version) {
        c.addClass("alert-success");
      }
    }
  });

  // Disable activity indicator.  We delay this by 300ms so that the activity
  // indicator is perceived on instant responses.
  setTimeout(function() {
    $('th.latest').activity(false);
  }, 300);
}
