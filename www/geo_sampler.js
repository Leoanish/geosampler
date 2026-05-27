(function(global) {
  var DEFAULT_RELOAD_TEXT =
    'Clearing maps, layers, caches, and memory. Please do not use the app for 10–15 seconds while the session restarts.';

  function reportClientError(source, message, detail) {
    if (typeof Shiny === 'undefined' || !Shiny.setInputValue) return;
    try {
      Shiny.setInputValue('geosampler_client_error', {
        source: source || 'browser',
        message: String(message || 'Browser-side error'),
        detail: detail ? String(detail).slice(0, 700) : '',
        at: new Date().toISOString(),
        nonce: Math.random()
      }, { priority: 'event' });
    } catch (e) {}
  }

  global.addEventListener('error', function(event) {
    reportClientError(
      'window.onerror',
      event && event.message ? event.message : 'Browser script error',
      event && event.filename ? (event.filename + ':' + event.lineno + ':' + event.colno) : ''
    );
  });

  global.addEventListener('unhandledrejection', function(event) {
    var reason = event && event.reason;
    reportClientError(
      'unhandledrejection',
      reason && reason.message ? reason.message : reason,
      reason && reason.stack ? reason.stack : ''
    );
  });

  function getReloadOverlayEl() {
    return document.getElementById('geosampler-reload-overlay');
  }

  function mountReloadOverlayOnBody() {
    var overlay = getReloadOverlayEl();
    if (!overlay) return null;
    if (overlay.parentNode !== document.body) {
      document.body.appendChild(overlay);
    }
    return overlay;
  }

  function setReloadStep(step) {
    var overlay = getReloadOverlayEl();
    if (!overlay) return;
    var steps = overlay.querySelectorAll('.geosampler-reload-step');
    if (!steps || !steps.length) return;
    var active = parseInt(step, 10);
    if (!isFinite(active) || active < 1) active = 1;
    steps.forEach(function(li) {
      var n = parseInt(li.getAttribute('data-step') || '0', 10);
      li.classList.toggle('is-active', n === active);
      li.classList.toggle('is-done', n < active);
    });
  }

  function showReloadOverlay(text, step) {
    var overlay = mountReloadOverlayOnBody();
    if (!overlay) return;
    document.body.classList.add('geosampler-reload-lock');
    overlay.classList.add('geosampler-reload-visible');
    overlay.setAttribute('aria-hidden', 'false');
    if (text) {
      var el = overlay.querySelector('.geosampler-reload-overlay-text');
      if (el) el.textContent = text;
    }
    if (step != null) setReloadStep(step);
  }

  var reloadOverlaySafetyTimer = null;

  function clearReloadOverlayTimers() {
    if (reloadOverlaySafetyTimer) {
      clearTimeout(reloadOverlaySafetyTimer);
      reloadOverlaySafetyTimer = null;
    }
  }

  function hideReloadOverlay() {
    var overlay = getReloadOverlayEl();
    clearReloadOverlayTimers();
    if (!overlay) {
      try {
        sessionStorage.removeItem('geosampler_reload_active');
        sessionStorage.removeItem('geosampler_just_reloaded');
      } catch (e) {}
      document.body.classList.remove('geosampler-reload-lock');
      return;
    }
    document.body.classList.remove('geosampler-reload-lock');
    overlay.classList.remove('geosampler-reload-visible');
    overlay.setAttribute('aria-hidden', 'true');
    try {
      sessionStorage.removeItem('geosampler_reload_active');
      sessionStorage.removeItem('geosampler_just_reloaded');
    } catch (e) {}
  }

  function scheduleReloadOverlaySafetyHide(ms) {
    clearReloadOverlayTimers();
    reloadOverlaySafetyTimer = setTimeout(function() {
      hideReloadOverlay();
    }, ms);
  }

  function bindReloadOverlayHandler() {
    if (global.__geosamplerReloadHandlerBound) return;
    if (typeof Shiny === 'undefined' || !Shiny.addCustomMessageHandler) return;
    global.__geosamplerReloadHandlerBound = true;
    Shiny.addCustomMessageHandler('geosamplerSessionReload', function(msg) {
      if (!msg) return;
      if (msg.show) {
        var pending = false;
        try {
          pending =
            !!sessionStorage.getItem('geosampler_reload_active') ||
            !!sessionStorage.getItem('geosampler_just_reloaded');
        } catch (e) {}
        if (!pending) return;
        showReloadOverlay(msg.text || DEFAULT_RELOAD_TEXT, msg.step);
      } else {
        hideReloadOverlay();
      }
    });
  }

  function resumeReloadOverlayAfterNavigation() {
    var pending = false;
    try {
      pending =
        !!sessionStorage.getItem('geosampler_just_reloaded') ||
        !!sessionStorage.getItem('geosampler_reload_active');
    } catch (e) {}
    if (!pending) return;
    showReloadOverlay(
      'Reconnecting and finishing session reset — please wait a few more seconds…',
      3
    );
    scheduleReloadOverlaySafetyHide(6000);
  }

  function onShinyConnectedAfterReload() {
    var pending = false;
    try {
      pending =
        !!sessionStorage.getItem('geosampler_just_reloaded') ||
        !!sessionStorage.getItem('geosampler_reload_active');
    } catch (e) {}
    if (!pending) {
      finishReloadOverlay();
      return;
    }
    showReloadOverlay(
      'Reconnecting and finishing session reset — please wait a few more seconds…',
      3
    );
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      Shiny.setInputValue('geosampler_post_reload_ping', Date.now(), {priority: 'event'});
    }
    scheduleReloadOverlaySafetyHide(1800);
    setTimeout(function() {
      finishReloadOverlay();
    }, 2200);
  }

  function finishReloadOverlay() {
    hideReloadOverlay();
  }

  global.GeoSamplerReloadOverlay = {
    show: showReloadOverlay,
    hide: hideReloadOverlay,
    finish: finishReloadOverlay,
    onConnected: onShinyConnectedAfterReload,
    setStep: setReloadStep,
    begin: function(text) {
      clearReloadOverlayTimers();
      try {
        sessionStorage.setItem('geosampler_reload_active', '1');
        sessionStorage.removeItem('geosampler_just_reloaded');
      } catch (e) {}
      showReloadOverlay(text || DEFAULT_RELOAD_TEXT, 1);
    }
  };

  document.addEventListener('DOMContentLoaded', function() {
    resumeReloadOverlayAfterNavigation();
    bindReloadOverlayHandler();
  });
  document.addEventListener('shiny:initialized', bindReloadOverlayHandler);
  if (typeof Shiny !== 'undefined' && Shiny.addCustomMessageHandler) {
    bindReloadOverlayHandler();
  }
})(window);

(function(global) {
  function bindGeoSamplerUiHandlers() {
    if (global.__geoSamplerUiHandlersBound) return true;
    if (typeof Shiny === 'undefined' || !Shiny.addCustomMessageHandler || !global.jQuery) return false;
    global.__geoSamplerUiHandlersBound = true;
    var $ = global.jQuery;

      function markTabRails() {
        var main = document.getElementById('main_tabs');
        if (main) {
          main.classList.add('geo-main-tabs');
        }
        ['variables_subtabs', 'sampling_subtabs', 'field_compare_subtabs', 'imagery_view_subtabs', 'summary_subtabs'].forEach(function(id) {
          var root = document.getElementById(id);
          if (!root) return;
          root.classList.add('geo-subtabs-root');
          var nav = root.matches && root.matches('ul.nav-tabs') ? root : root.querySelector('ul.nav-tabs');
          if (nav) nav.classList.add('geo-subtabs');
        });
      }

      Shiny.addCustomMessageHandler('getLocation', function(message) {
        if (navigator.geolocation) {
          navigator.geolocation.getCurrentPosition(function(position) {
            Shiny.setInputValue('user_lat', position.coords.latitude);
            Shiny.setInputValue('user_lon', position.coords.longitude);
          }, function(error) {
            alert('Unable to retrieve your location: ' + error.message);
          });
        } else {
          alert('Geolocation is not supported by your browser.');
        }
      });

      // Reliable Leaflet Draw hook for the boundary editor (map-map): attach once the
      // htmlwidget map exists (Shiny Leaflet stores the map on the widget instance).
      function getLeafletMapByEl(el) {
        if (!el) return null;
        if (window.HTMLWidgets && HTMLWidgets.findElement) {
          var w = HTMLWidgets.findElement(el);
          if (w && typeof w.getMap === 'function') return w.getMap();
        }
        return el._leaflet_map || null;
      }
      function hookBoundaryDrawDelete() {
        var el = document.getElementById('map-map');
        var map = getLeafletMapByEl(el);
        if (!map) return false;
        if (map._boundaryDrawDeleteHooked) return true;
        map._boundaryDrawDeleteHooked = true;
        map._boundaryDrawCount = 0;
        map.on('draw:deleted', function() {
          map._boundaryDrawCount = 0;
          Shiny.setInputValue('boundary_draw_deleted', Math.random(), {priority: 'event'});
        });
        map.on('draw:created', function(e) {
          map._boundaryDrawCount = (map._boundaryDrawCount || 0) + 1;
          if (map._boundaryDrawCount > 1) {
            if (e.layer && map.hasLayer && map.hasLayer(e.layer)) {
              try { map.removeLayer(e.layer); } catch (err) {}
            }
            map._boundaryDrawCount = 1;
            Shiny.setInputValue('too_many_drawn_polygons', Math.random(), {priority: 'event'});
          }
        });
        return true;
      }
      $(document).on('shiny:connected', function() {
        var t = setInterval(function() {
          if (hookBoundaryDrawDelete()) clearInterval(t);
        }, 400);
      });
      Shiny.addCustomMessageHandler('clearLeafletDrawFeatures', function(msg) {
        var el = document.getElementById(msg.mapId || 'map-map');
        var map = getLeafletMapByEl(el);
        if (!map) return;
        map._boundaryDrawCount = 0;
        map.eachLayer(function(layer) {
          try {
            if (layer instanceof L.FeatureGroup && layer.getLayers) {
              layer.clearLayers();
            }
          } catch (e) {}
        });
      });
      Shiny.addCustomMessageHandler('markButtonUsed', function(msg) {
        if (!msg || !msg.id) return;
        var btn = document.getElementById(msg.id);
        if (!btn) return;
        btn.disabled = true;
        btn.setAttribute('aria-disabled', 'true');
        btn.classList.add('btn-action-used');
      });
      Shiny.addCustomMessageHandler('markButtonActive', function(msg) {
        if (!msg || !msg.id) return;
        var btn = document.getElementById(msg.id);
        if (!btn) return;
        btn.disabled = false;
        btn.removeAttribute('aria-disabled');
        btn.classList.remove('btn-action-used');
      });
      Shiny.addCustomMessageHandler('geosamplerReenableAllButtons', function() {
        document.querySelectorAll('.btn-action-used').forEach(function(btn) {
          btn.disabled = false;
          btn.removeAttribute('aria-disabled');
          btn.classList.remove('btn-action-used');
        });
      });
      Shiny.addCustomMessageHandler('invalidateLeafletMaps', function(msg) {
        var ids = (msg && msg.mapIds) ? msg.mapIds : [];
        ids.forEach(function(id) {
          var el = document.getElementById(id);
          var map = getLeafletMapByEl(el);
          if (map && map.invalidateSize) {
            try { map.invalidateSize(); } catch (e) {}
          }
        });
      });
      Shiny.addCustomMessageHandler('fitLeafletMapsToBounds', function(msg) {
        if (!msg || !msg.bounds) return;
        var b = msg.bounds;
        var sw = L.latLng(b.ymin, b.xmin);
        var ne = L.latLng(b.ymax, b.xmax);
        var ids = msg.mapIds || [];
        var fit = function(map) {
          if (!map || !map.fitBounds) return;
          map.invalidateSize();
          map.fitBounds(L.latLngBounds(sw, ne), {padding: [24, 24], maxZoom: 18});
        };
        ids.forEach(function(id) {
          var el = document.getElementById(id);
          var map = getLeafletMapByEl(el);
          if (!map) return;
          if (msg.delayFit) {
            setTimeout(function() { fit(getLeafletMapByEl(el)); }, 150);
            setTimeout(function() { fit(getLeafletMapByEl(el)); }, 550);
          } else {
            fit(map);
          }
        });
      });
      Shiny.addCustomMessageHandler('forceDashboardStatus', function(msg) {
        var el = document.getElementById('force-dashboard-status');
        if (!el) return;
        var text = (msg && msg.text) ? String(msg.text) : '';
        if (!text) {
          el.style.display = 'none';
          el.textContent = '';
          return;
        }
        el.textContent = text;
        el.style.display = 'block';
      });
      Shiny.addCustomMessageHandler('setSentinelTimeseriesVisible', function(msg) {
        var show = !!(msg && msg.visible);
        document.body.classList.toggle('sentinel-timeseries-visible', show);
      });

      var __tabTipEl = null;
      function ensureTabTipEl() {
        if (__tabTipEl) return __tabTipEl;
        __tabTipEl = document.getElementById('geosampler-hover-tip');
        if (__tabTipEl) return __tabTipEl;
        __tabTipEl = document.createElement('div');
        __tabTipEl.id = 'geosampler-hover-tip';
        __tabTipEl.style.position = 'fixed';
        __tabTipEl.style.zIndex = '99999';
        __tabTipEl.style.pointerEvents = 'none';
        __tabTipEl.style.background = 'rgba(24,29,35,0.96)';
        __tabTipEl.style.color = '#fff';
        __tabTipEl.style.padding = '8px 11px';
        __tabTipEl.style.borderRadius = '10px';
        __tabTipEl.style.fontSize = '13.5px';
        __tabTipEl.style.lineHeight = '1.35';
        __tabTipEl.style.fontWeight = '650';
        __tabTipEl.style.boxShadow = '0 12px 28px rgba(20,25,32,0.26)';
        __tabTipEl.style.maxWidth = '460px';
        __tabTipEl.style.wordWrap = 'break-word';
        __tabTipEl.style.overflowWrap = 'break-word';
        __tabTipEl.style.display = 'none';
        document.body.appendChild(__tabTipEl);
        return __tabTipEl;
      }
      function showTabTip(e, txt) {
        var el = ensureTabTipEl();
        el.textContent = txt;
        el.style.left = (e.clientX + 12) + 'px';
        el.style.top = (e.clientY + 12) + 'px';
        el.style.display = 'block';
      }
      function hideTabTip() {
        var el = ensureTabTipEl();
        el.style.display = 'none';
      }
      function bindTabTipAnchor(a, txt) {
        if (!a || !txt) return;
        a.removeAttribute('title');
        a.setAttribute('data-tab-tip', txt);
        if (!a.dataset.tipBound) {
          a.addEventListener('mouseenter', function(e) { showTabTip(e, a.getAttribute('data-tab-tip') || ''); });
          a.addEventListener('mousemove', function(e) { showTabTip(e, a.getAttribute('data-tab-tip') || ''); });
          a.addEventListener('mouseleave', hideTabTip);
          a.dataset.tipBound = '1';
        }
      }
      function applyTabTooltips() {
        var tipByLabel = {
          'Welcome': 'Overview of the app workflow and guidance.',
          'Dashboard': 'Overview of the app workflow and guidance.',
          'Boundary': 'Define one AOI (draw or upload). All maps and downloads use this boundary.',
          'Variables & derivatives': 'Imagery, elevation, other layers, and variable summary—build covariates before sampling.',
          'Variables': 'Same as Variables & derivatives (legacy label).',
          'Imagery': 'Planet, Sentinel-2, or upload GeoTIFF; compute VIs for sampling. Large AOIs can stress small hosting.',
          'Imagery Analysis': 'Same as Imagery (legacy label).',
          'Elevation': 'Retrieve or upload one DEM, then Calculate Slope, Aspect, TPI, TWI in one click.',
          'Other layers': 'Upload extra GeoTIFF predictors (soil, climate, custom layers).',
          'Other Layers': 'Same as Other layers (legacy label).',
          'Variable summary': 'Min, max, mean, median per layer inside the AOI (QA before sampling).',
          'Variable Summary': 'Same as Variable summary (legacy label).',
          'Sampling': 'Technique comparison, generate points, and population vs sample review.',
          'Technique comparison': 'Compare six designs across sample sizes; optional before generating points.',
          'Technique Comparison': 'Same as Technique comparison (legacy label).',
          'Generate sample points': 'Manual or automatic point generation, map review, and GeoJSON export.',
          'Population vs sample': 'Population vs sample statistics and dual-violin distribution plots.',
          'Summary': 'Same as Population vs sample (legacy label).',
          'App vs prior': 'Compare historical/prior field GPS points with the new app sample plan.',
          'Field': 'Historical field GPS vs app samples—map and covariate distributions.',
          'Field comparison': 'Historical field GPS vs app samples—map and covariate distributions.',
          'Field Comparison': 'Same as Field comparison (legacy label).',
          'Cost': 'Total sampling cost for prior grid design vs app-recommended sample size.',
          'Cost comparison': 'Total sampling cost for prior grid design vs app-recommended sample size.',
          'Cost Comparison': 'Same as Cost comparison (legacy label).',
          'Report (PDF)': 'One-page field sampling PDF for planning and sharing.',
          'Report': 'Same as Report (PDF) (legacy label).'
        };
        var nestedTips = {
          field_compare_subtabs: {
            'Map': 'Side-by-side map of app-recommended sample points (blue) and uploaded historical / previously collected field points (orange). Check spatial overlap and gaps before going to the field.',
            'Distributions': 'Per-layer violin plots of extracted covariate values. Compare whether app points and historical points sample similar ranges; dashed red/orange lines show the mean per source.'
          },
          variables_subtabs: {
            'Imagery': 'Retrieve Planet or Sentinel-2 imagery, or upload your own multispectral GeoTIFFs, then calculate vegetation indices for sampling.',
            'Elevation': 'Retrieve or upload elevation data, then calculate terrain derivatives such as slope, aspect, TPI, and TWI.',
            'Other layers': 'Upload supporting GeoTIFF predictors such as soil, EC, management, or climate layers.',
            'Variable summary': 'Review min, max, mean, and median for loaded layers inside the active boundary before sampling.'
          },
          sampling_subtabs: {
            'Technique comparison': 'Compare candidate sampling designs and sample sizes to choose a recommended method and n.',
            'Generate sample points': 'Create manual or automatic sample points, review them on the map, and export the final plan.',
            'Population vs sample': 'Check how well sample point covariates represent the full field population.'
          },
          imagery_view_subtabs: {
            'Timeseries viewer': 'Search available Sentinel dates, then click Build timeseries to review the NDRE trend and pick a date to retrieve.',
            'Imagery Viewer': 'Retrieve the scene for the date you chose from the time series (map, bands, VI).'
          },
          summary_subtabs: {
            'Table': 'Population (all AOI raster cells) vs sample (your current points) min, max, mean, and median for each covariate layer.',
            'Distribution': 'Per-layer overlaid violins: gray population cloud vs blue app sample distribution, with colored points on top.'
          }
        };
        var anchors = document.querySelectorAll('.nav.nav-tabs a');
        anchors.forEach(function(a) {
          var embedded = a.querySelector('[data-tab-tip]');
          if (embedded) {
            bindTabTipAnchor(a, embedded.getAttribute('data-tab-tip') || '');
            return;
          }
          var raw = (a.textContent || '').replace(/[\u200B-\u200D\uFEFF]/g, '').trim().replace(/\s+/g, ' ');
          var label = raw;
          if (!tipByLabel[label]) {
            var keys = Object.keys(tipByLabel).sort(function(x, y) { return y.length - x.length; });
            for (var i = 0; i < keys.length; i++) {
              if (raw.indexOf(keys[i]) !== -1) {
                label = keys[i];
                break;
              }
            }
          }
          if (tipByLabel[label]) bindTabTipAnchor(a, tipByLabel[label]);
        });
        Object.keys(nestedTips).forEach(function(panelId) {
          var root = document.getElementById(panelId);
          if (!root) return;
          var nav = root.querySelector('ul.nav-tabs');
          if (!nav) return;
          var map = nestedTips[panelId];
          nav.querySelectorAll('a').forEach(function(a) {
            var lbl = (a.textContent || '').replace(/[\u200B-\u200D\uFEFF]/g, '').trim().replace(/\s+/g, ' ');
            if (map[lbl]) bindTabTipAnchor(a, map[lbl]);
          });
        });
        applyWorkflowTreeTooltips();
      }

      function currentHoverTip(el) {
        if (!el) return '';
        var direct = el.getAttribute && (el.getAttribute('data-tab-tip') || el.getAttribute('data-wf-tip'));
        if (direct) return direct;
        var embedded = el.querySelector && el.querySelector('[data-tab-tip]');
        if (embedded) return embedded.getAttribute('data-tab-tip') || '';
        return '';
      }

      function closestTipTarget(start) {
        var el = start;
        while (el && el !== document && el.nodeType === 1) {
          if (el.matches && (
            el.matches('.nav-tabs a') ||
            el.matches('[role="tab"]') ||
            el.matches('[data-tab-tip]') ||
            el.matches('[data-wf-tip]') ||
            el.matches('.workflow-tree-btn[data-wf-tip]') ||
            el.matches('.wf-node-hint')
          )) {
            return el;
          }
          el = el.parentElement;
        }
        return null;
      }

      function inferTipFromLabel(el) {
        if (!el) return '';
        var raw = (el.textContent || '').replace(/[\u200B-\u200D\uFEFF]/g, '').trim().replace(/\s+/g, ' ');
        if (!raw) return '';
        var tips = {
          'Welcome': 'Overview of the app workflow and guidance.',
          'Dashboard': 'Overview of the app workflow and guidance.',
          'Boundary': 'Define one AOI boundary by drawing or uploading; all maps, samples, and exports use this field.',
          'Variables & derivatives': 'Load imagery, elevation, and other raster layers, then derive covariates for sampling.',
          'Imagery': 'Search/retrieve Sentinel or Planet imagery, or upload GeoTIFFs, then calculate vegetation indices.',
          'Elevation': 'Retrieve or upload elevation data and calculate terrain derivatives.',
          'Other layers': 'Upload additional GeoTIFF predictors such as soil, EC, or management layers.',
          'Variable summary': 'Review layer statistics inside the boundary before using them for sampling.',
          'Sampling': 'Compare sampling designs, generate sample points, and review population vs sample balance.',
          'Technique comparison': 'Compare candidate designs and sample sizes to choose a recommended method and n.',
          'Generate sample points': 'Create manual or automatic sample locations and export the field plan.',
          'Population vs sample': 'Compare sampled covariate values against the full field population.',
          'Table': 'View population and sample summary statistics for each covariate layer.',
          'Distribution': 'View distribution plots comparing field population values and sample point values.',
          'Timeseries viewer': 'Search Sentinel dates, then build the NDRE timeseries to pick a scene.',
          'Imagery Viewer': 'Retrieve and inspect the selected imagery scene or composite.',
          'App vs prior': 'Compare historical/prior field GPS points with the new app sample plan.',
          'Field': 'Compare historical field GPS points with the new app sample plan.',
          'Field comparison': 'Compare historical field GPS points with the new app sample plan.',
          'Map': 'View app sample points and uploaded historical field points together on the map.',
          'Distributions': 'Compare covariate distributions for historical points versus app sample points.',
          'Cost': 'Estimate total sampling cost and percent savings for prior grid sampling versus the app plan.',
          'Cost comparison': 'Estimate total sampling cost and percent savings for prior grid sampling versus the app plan.',
          'Report (PDF)': 'Download a PDF report with maps, comparison plots, costs, and sampling details.'
        };
        if (tips[raw]) return tips[raw];
        var keys = Object.keys(tips).sort(function(a, b) { return b.length - a.length; });
        for (var i = 0; i < keys.length; i++) {
          if (raw.indexOf(keys[i]) !== -1) return tips[keys[i]];
        }
        return '';
      }

      var __lastTipTarget = null;
      function handleGlobalTipMove(e) {
        var target = closestTipTarget(e.target);
        if (!target) {
          if (__lastTipTarget) hideTabTip();
          __lastTipTarget = null;
          return;
        }
        var tip = currentHoverTip(target) || target.getAttribute('title') || inferTipFromLabel(target);
        if (!tip) return;
        target.setAttribute('data-tab-tip', tip);
        target.removeAttribute('title');
        __lastTipTarget = target;
        showTabTip(e, tip);
      }

      document.addEventListener('mousemove', handleGlobalTipMove, true);
      document.addEventListener('mouseover', handleGlobalTipMove, true);
      document.addEventListener('mouseout', function(e) {
        if (__lastTipTarget && (!e.relatedTarget || !__lastTipTarget.contains(e.relatedTarget))) {
          hideTabTip();
          __lastTipTarget = null;
        }
      }, true);

      function applyWorkflowTreeTooltips() {
        document.querySelectorAll('.workflow-tree-btn[data-wf-tip]').forEach(function(btn) {
          bindTabTipAnchor(btn, btn.getAttribute('data-wf-tip') || '');
        });
        document.querySelectorAll('.wf-node-hint[title]').forEach(function(el) {
          bindTabTipAnchor(el, el.getAttribute('title') || '');
        });
      }

      $(document).on('shiny:connected', function() {
        setTimeout(applyTabTooltips, 500);
        setTimeout(applyWorkflowTreeTooltips, 650);
        setTimeout(markTabRails, 350);
      });
      $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"], a[data-bs-toggle=\"tab\"]', function() {
        setTimeout(applyTabTooltips, 40);
        applyTabTooltips();
        markTabRails();
        Shiny.setInputValue('map_tab_shown', Date.now(), {priority: 'event'});
        var href = $(this).attr('href');
        if (href) {
          var pane = $(href);
          if (pane.length) {
            pane.addClass('geo-tab-enter');
            setTimeout(function() { pane.removeClass('geo-tab-enter'); }, 320);
          }
        }
      });
      $(document).on('mouseenter', '.nav.nav-tabs a, .workflow-tree-btn[data-wf-tip], .wf-node-hint[title]', function(e) {
        applyTabTooltips();
        var tip = currentHoverTip(this);
        if (tip) showTabTip(e, tip);
      });
      $(document).on('mousemove', '.nav.nav-tabs a, .workflow-tree-btn[data-wf-tip], .wf-node-hint[title]', function(e) {
        var tip = currentHoverTip(this);
        if (tip) showTabTip(e, tip);
      });
      $(document).on('mouseleave', '.nav.nav-tabs a, .workflow-tree-btn[data-wf-tip], .wf-node-hint[title]', function() {
        hideTabTip();
      });
      $(document).on('mouseenter', '#variables_summary_table_out table tbody tr', function(e) {
        var t = this.getAttribute('data-row-tip');
        if (t) showTabTip(e, t);
      });
      $(document).on('mousemove', '#variables_summary_table_out table tbody tr', function(e) {
        var t = this.getAttribute('data-row-tip');
        if (t) showTabTip(e, t);
      });
      $(document).on('mouseleave', '#variables_summary_table_out table tbody tr', function() {
        hideTabTip();
      });

      $(document).on('shiny:connected', function() {
        if (window.GeoSamplerReloadOverlay && window.GeoSamplerReloadOverlay.onConnected) {
          window.GeoSamplerReloadOverlay.onConnected();
        }
        document.querySelectorAll('.btn-action-used').forEach(function(btn) {
          btn.disabled = false;
          btn.removeAttribute('aria-disabled');
          btn.classList.remove('btn-action-used');
        });
      });

      setTimeout(applyTabTooltips, 250);
      setTimeout(applyWorkflowTreeTooltips, 300);
      setTimeout(markTabRails, 350);

    return true;
  }

  function bindWhenReady() {
    if (bindGeoSamplerUiHandlers()) return;
    setTimeout(bindWhenReady, 120);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', bindWhenReady);
  } else {
    bindWhenReady();
  }
  document.addEventListener('shiny:initialized', bindWhenReady);
})(window);

(function(global) {
  var tipEl = null;
  var activeTarget = null;
  var labelTips = {
    'Dashboard': 'Overview of the app workflow and guidance.',
    'Welcome': 'Overview of the app workflow and guidance.',
    'Boundary': 'Define one AOI boundary by drawing or uploading; all maps, samples, and exports use this field.',
    'Variables': 'Load imagery, elevation, and other raster layers, then derive covariates for sampling.',
    'Imagery': 'Retrieve Planet or Sentinel-2 imagery, or upload GeoTIFFs, then calculate vegetation indices for sampling.',
    'Elevation': 'Retrieve or upload elevation data and calculate terrain derivatives.',
    'Other layers': 'Upload supporting GeoTIFF predictors such as soil, EC, management, or climate layers.',
    'Variable summary': 'Review layer statistics inside the boundary before using them for sampling.',
    'Sampling': 'Compare sampling designs, generate sample points, and review population vs sample balance.',
    'Technique comparison': 'Compare candidate sampling designs and sample sizes to choose a recommended method and n.',
    'Generate sample points': 'Create manual or automatic sample locations and export the field plan.',
    'Population vs sample': 'Compare sampled covariate values against the full field population.',
    'App vs prior': 'Compare historical/prior field GPS points with the new app sample plan.',
    'Field': 'Compare historical field GPS points with the new app sample plan.',
    'Map': 'View app sample points and uploaded historical field points together on the map.',
    'Distributions': 'Compare covariate distributions for historical points versus app sample points.',
    'Cost': 'Estimate total sampling cost and percent savings for prior grid sampling versus the app plan.',
    'Report': 'Download a PDF report with maps, comparison plots, costs, and sampling details.',
    'Timeseries viewer': 'Search Sentinel dates, then build the NDRE timeseries to pick a scene.',
    'Imagery Viewer': 'Retrieve and inspect the selected imagery scene or composite.'
  };

  function getTipEl() {
    if (tipEl) return tipEl;
    tipEl = document.getElementById('geosampler-hover-tip');
    if (!tipEl) {
      tipEl = document.createElement('div');
      tipEl.id = 'geosampler-hover-tip';
      tipEl.style.position = 'fixed';
      tipEl.style.zIndex = '2147482';
      tipEl.style.pointerEvents = 'none';
      tipEl.style.background = 'rgba(24,29,35,0.96)';
      tipEl.style.color = '#fff';
      tipEl.style.padding = '8px 11px';
      tipEl.style.borderRadius = '10px';
      tipEl.style.fontSize = '13.5px';
      tipEl.style.lineHeight = '1.35';
      tipEl.style.fontWeight = '650';
      tipEl.style.boxShadow = '0 12px 28px rgba(20,25,32,0.26)';
      tipEl.style.maxWidth = '460px';
      tipEl.style.wordWrap = 'break-word';
      tipEl.style.overflowWrap = 'break-word';
      tipEl.style.display = 'none';
      document.body.appendChild(tipEl);
    }
    return tipEl;
  }

  function normalizedText(el) {
    return (el && el.textContent ? el.textContent : '')
      .replace(/[\u200B-\u200D\uFEFF]/g, '')
      .trim()
      .replace(/\s+/g, ' ');
  }

  function findTarget(start) {
    var el = start;
    while (el && el !== document && el.nodeType === 1) {
      if (el.matches && (
        el.matches('.nav-tabs a') ||
        el.matches('[role="tab"]') ||
        el.matches('[data-tab-tip]') ||
        el.matches('[data-wf-tip]') ||
        el.matches('.workflow-tree-btn') ||
        el.matches('.wf-node-hint')
      )) return el;
      el = el.parentElement;
    }
    return null;
  }

  function inferTip(el) {
    if (!el) return '';
    var direct = el.getAttribute('data-tab-tip') || el.getAttribute('data-wf-tip') || el.getAttribute('title');
    if (direct) return direct;
    var embedded = el.querySelector('[data-tab-tip],[data-wf-tip],[title]');
    if (embedded) return embedded.getAttribute('data-tab-tip') || embedded.getAttribute('data-wf-tip') || embedded.getAttribute('title') || '';
    var raw = normalizedText(el);
    if (labelTips[raw]) return labelTips[raw];
    var keys = Object.keys(labelTips).sort(function(a, b) { return b.length - a.length; });
    for (var i = 0; i < keys.length; i++) {
      if (raw.indexOf(keys[i]) !== -1) return labelTips[keys[i]];
    }
    return '';
  }

  function show(e, text) {
    var el = getTipEl();
    el.textContent = text;
    el.style.left = Math.min(e.clientX + 14, window.innerWidth - 480) + 'px';
    el.style.top = Math.min(e.clientY + 14, window.innerHeight - 90) + 'px';
    el.style.display = 'block';
  }

  function hide() {
    var el = getTipEl();
    el.style.display = 'none';
    activeTarget = null;
  }

  function onMove(e) {
    var target = findTarget(e.target);
    if (!target) return hide();
    var tip = inferTip(target);
    if (!tip) return hide();
    target.setAttribute('data-tab-tip', tip);
    target.removeAttribute('title');
    activeTarget = target;
    show(e, tip);
  }

  document.addEventListener('pointermove', onMove, true);
  document.addEventListener('mouseover', onMove, true);
  document.addEventListener('mouseout', function(e) {
    if (activeTarget && (!e.relatedTarget || !activeTarget.contains(e.relatedTarget))) hide();
  }, true);
})(window);