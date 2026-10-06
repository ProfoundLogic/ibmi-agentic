
function sleep(ms) {
  return new Promise(
    resolve => setTimeout(resolve, ms)
  );
}

function pressWait(key) {
  // wait until server is not busy before proceeding to the next step
  if (pui.isServerBusy()) {
    // check again in 1/10th of a second
    setTimeout(() => {
      pressWait(key);
    }, 100);
  }
  else {
      presskey(key);
  }
}

function serverReady(seconds) {
  return new Promise(async (resolve, reject) => {
    for (let ctr = seconds * 10; ctr > 0; ctr -= 1) {
      if (pui.isServerBusy()) {
        await sleep(100);
      }
      else {
        resolve();
      }
    }
    reject('Server not responding!');
  });
}

function restoreSearch(search) {
  let saveobj = JSON.parse(search);
  Object.entries(saveobj).forEach(([key, value]) => {
    if (getObj(key)) {
      pui.set(key, get(value, true) ? get(value, true) : value);
      // Fire the onchange event
      getObj(key).dispatchEvent(new Event('change'));
    }
  })
  // Wait for onchange events to finish
  setTimeout(() => {pui.click("CTLREFRESH")}, 500);
}

function applyPropertyCSS(selector, propertyName, value) {
  let elemList = Array.from(document.querySelectorAll(selector));
  elemList.forEach(elem => {
    applyProperty(elem.id, propertyName, value);
  });
}

// Used with the "user defined data" PUI properties to resture css class, widget types
function applyPropertyCSSfromPUI(selector, propertyName, puiProperty, row = 0) {
  let elemList = Array.from(document.querySelectorAll(selector));
  if (row > 0) {
    elemList = elemList.filter(elem => elem.id.endsWith(`.${row}`));
  }
  elemList.forEach(elem => {
    applyProperty(elem.id, propertyName, getObj(elem.id).pui.properties[puiProperty]);
  });
}

function sanitizeFilename(filename) {
  // Remove illegal characters for standard file systems
  // \x00-\x1f : Control characters
  // / ? < > \ : * | " : & Forbidden characters in Windows/Mac/Linux
  let sanitized = filename.replace(/[\x00-\x1f\\/?<>:*|"&]/g, '_');

  // Prevent directory traversal (e.g., ../ or ..\)
  sanitized = sanitized.replace(/^\.+/g, '');

  return sanitized
}

pui["brkmsg enable"] = true;
pui["brkmsg poll interval"] = 1;
//pui["autocomplete tab selects"] = true;
pui["grid text selection"] = true;
pui["highlight on focus"] = true;