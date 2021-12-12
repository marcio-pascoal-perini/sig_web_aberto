$(document).on(
  'shiny:connected',
  function () {
    $('*').css('cursor', 'wait');
  }
);

$(document).on(
  'shiny:busy',
  function () {
    $('*').css('cursor', 'wait');
  }
);

$(document).ajaxStart(
  function () {
    $('*').css('cursor', 'wait');
  }
);

$(document).ajaxStop(
  function () {
    if (!$('html').hasClass('shiny-busy')) {
      $('*').css('cursor', 'default');
    }
  }
);

$(document).on(
  'shiny:idle',
  function () {
    $('*').css('cursor', 'default');
  }
);

$(document).ready(function () {
  var div1 = $(document.createElement('div'))
    .attr('id', 'cortina')
    .css('display', 'none')
    .css('position', 'fixed')
    .css('margin', '0')
    .css('padding', '0')
    .css('top', '0')
    .css('left', '0')
    .css('background', 'rgba(245, 245, 245, 0.8)')
    .css('width', '100%')
    .css('height', '100%')
    .css('z-index', '9999');
  var div2 = $(document.createElement('div'))
    .css('position', 'absolute')
    .css('width', '100px')
    .css('height', '100px')
    .css('background', '#ffffff')
    .css('text-align', 'center')
    .css('left', '50%')
    .css('top', '50%')
    .css('margin-left', '-50px')
    .css('margin-top', '-50px')
    .css('vertical-align', 'middle')
    .css('border', '2px solid #a9a9a9')
    .css('border-radius', '8px');
  var img = $(document.createElement('img'))
    .attr('src', 'loading2.gif')
    .css('position', 'absolute')
    .css('border', 'none')
    .css('margin', 'auto')
    .css('top', '0')
    .css('left', '0')
    .css('right', '0')
    .css('bottom', '0')
    .css('width', '60%')
    .css('height', '60%');
  div2.append(img);
  div1.append(div2);
  $('body').append(div1);
});
