(function () {
  "use strict";

  var scheduled = false;

  function textOf(element) {
    return element
      ? element.textContent.trim().replace(/\s+/g, " ").toLowerCase()
      : "";
  }

  function findResultsItem() {
    var links = document.querySelectorAll(
      ".navbar .navbar-nav .nav-link"
    );

    for (var i = 0; i < links.length; i += 1) {
      if (textOf(links[i]) === "results") {
        return links[i].closest("li") || links[i];
      }
    }

    return null;
  }

  function findNavbarList() {
    var resultsItem = findResultsItem();

    if (resultsItem && resultsItem.parentElement) {
      return resultsItem.parentElement;
    }

    return document.querySelector(".navbar .navbar-nav");
  }

  function getOrCreateSlot() {
    var existing = document.getElementById(
      "ef-navbar-series-selector-slot"
    );

    if (existing) {
      return existing;
    }

    var navList = findNavbarList();
    var resultsItem = findResultsItem();

    if (!navList) {
      return null;
    }

    var slot = document.createElement("li");
    slot.id = "ef-navbar-series-selector-slot";
    slot.className = "nav-item ef-navbar-series-selector-slot";

    if (resultsItem && resultsItem.nextSibling) {
      navList.insertBefore(slot, resultsItem.nextSibling);
    } else {
      navList.appendChild(slot);
    }

    return slot;
  }

  function currentSelectorControl() {
    var controls = document.querySelectorAll(
      ".ef-global-series-switcher-control"
    );

    if (controls.length === 0) {
      return null;
    }

    for (var i = controls.length - 1; i >= 0; i -= 1) {
      if (
        controls[i].querySelector("#detail_series") ||
        controls[i].querySelector(".selectize-control")
      ) {
        return controls[i];
      }
    }

    return controls[controls.length - 1];
  }

  function cleanOldMountedControls(slot, keep) {
    var mounted = slot.querySelectorAll(
      ".ef-global-series-switcher-control"
    );

    mounted.forEach(function (element) {
      if (element !== keep) {
        element.remove();
      }
    });
  }

  function mountSelector() {
    scheduled = false;

    var slot = getOrCreateSlot();
    var control = currentSelectorControl();

    if (!slot || !control) {
      return;
    }

    cleanOldMountedControls(slot, control);

    if (control.parentElement !== slot) {
      slot.appendChild(control);
    }

    control.classList.add(
      "ef-navbar-series-selector-control"
    );

    var banners = document.querySelectorAll(
      ".ef-global-series-switcher"
    );

    banners.forEach(function (banner) {
      banner.classList.add(
        "ef-selector-banner-retired"
      );
    });

    slot.style.display = "";
  }

  function scheduleMount() {
    if (scheduled) {
      return;
    }

    scheduled = true;

    window.requestAnimationFrame(function () {
      mountSelector();
    });
  }

  document.addEventListener(
    "DOMContentLoaded",
    scheduleMount
  );

  document.addEventListener(
    "shown.bs.tab",
    scheduleMount
  );

  document.addEventListener(
    "click",
    function (event) {
      if (event.target.closest(".navbar .nav-link")) {
        window.setTimeout(scheduleMount, 0);
      }
    }
  );

  if (window.jQuery) {
    window.jQuery(document).on(
      "shiny:connected shiny:value shiny:recalculated shown.bs.tab",
      scheduleMount
    );
  }

  var observer = new MutationObserver(function () {
    scheduleMount();
  });

  observer.observe(document.documentElement, {
    childList: true,
    subtree: true
  });

  scheduleMount();
})();

/* EPIFORESS TRUE OVERLAY DROPDOWN - START */
(function () {
  "use strict";

  var overlayScheduled = false;

  function getSelectize() {
    var input = document.getElementById("detail_series");

    if (!input || !input.selectize) {
      return null;
    }

    return input.selectize;
  }

  function forceDropdownIntoBody() {
    overlayScheduled = false;

    var selectize = getSelectize();

    if (!selectize || !selectize.$dropdown) {
      return;
    }

    selectize.settings.dropdownParent = "body";

    var dropdown = selectize.$dropdown[0];

    if (dropdown && dropdown.parentElement !== document.body) {
      document.body.appendChild(dropdown);
    }

    if (dropdown) {
      dropdown.classList.add(
        "ef-navbar-series-dropdown-overlay"
      );
    }

    if (typeof selectize.positionDropdown === "function") {
      selectize.positionDropdown();
    }
  }

  function scheduleOverlayFix() {
    if (overlayScheduled) {
      return;
    }

    overlayScheduled = true;

    window.requestAnimationFrame(function () {
      forceDropdownIntoBody();
    });
  }

  function bindOverlayEvents() {
    var selectize = getSelectize();

    if (!selectize || selectize.__epiOverlayBound) {
      return;
    }

    selectize.__epiOverlayBound = true;

    selectize.on("dropdown_open", function () {
      forceDropdownIntoBody();

      window.setTimeout(function () {
        forceDropdownIntoBody();
      }, 0);
    });

    selectize.on("focus", function () {
      forceDropdownIntoBody();
    });

    selectize.on("change", function () {
      forceDropdownIntoBody();
    });

    forceDropdownIntoBody();
  }

  function installOverlayBehavior() {
    bindOverlayEvents();
    scheduleOverlayFix();
  }

  document.addEventListener(
    "DOMContentLoaded",
    installOverlayBehavior
  );

  document.addEventListener(
    "shown.bs.tab",
    installOverlayBehavior
  );

  window.addEventListener(
    "resize",
    scheduleOverlayFix
  );

  window.addEventListener(
    "scroll",
    scheduleOverlayFix,
    true
  );

  if (window.jQuery) {
    window.jQuery(document).on(
      "shiny:connected shiny:value shiny:recalculated shown.bs.tab",
      installOverlayBehavior
    );
  }

  var overlayObserver = new MutationObserver(function () {
    installOverlayBehavior();
  });

  overlayObserver.observe(document.documentElement, {
    childList: true,
    subtree: true
  });

  installOverlayBehavior();
})();
/* EPIFORESS TRUE OVERLAY DROPDOWN - END */
