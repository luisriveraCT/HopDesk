// search.js — column-sort state and header-click handler for the Search modal.
// Mirrors the vencidos.js sort pattern. Safe to re-execute on each modal open
// because function reassignment is idempotent and state is reset by the modal's
// own setTimeout (window._searchSortCol = null).

window._searchSortCol = null;
window._searchSortDir = 'asc';

// -- Month nav (static; pool data injected by inline script at modal open) ---
window.searchSetMonth = function(month, btn) {
  window._smCur = month;
  document.querySelectorAll('.search-month-nav .btn').forEach(function(b) {
    b.classList.remove('btn-primary');
    b.classList.add('btn-outline-secondary');
  });
  btn.classList.remove('btn-outline-secondary');
  btn.classList.add('btn-primary');
  if (window.searchFilterAndSort) window.searchFilterAndSort();
};

window.searchMonthShift = function(dir) {
  var pool   = window._smPool;
  var labels = window._smLabels;
  var newWin = (window._smWin || 0) + dir;
  if (!pool || newWin < 0 || newWin + 5 > pool.length) return;
  var nav  = document.getElementById('search-month-nav');
  var prev = document.getElementById('search-month-prev');
  var next = document.getElementById('search-month-next');
  if (!nav) return;
  nav.classList.remove('sm-out-l','sm-out-r','sm-in-l','sm-in-r');
  nav.classList.add(dir > 0 ? 'sm-out-l' : 'sm-out-r');
  setTimeout(function() {
    window._smWin = newWin;
    nav.classList.remove('sm-out-l','sm-out-r');
    // Pin opacity:0 before injecting buttons — prevents a single painted frame
    // of fully-visible buttons before the sm-in animation's `from` state kicks in.
    nav.style.opacity = '0';
    nav.innerHTML = '';
    for (var i = newWin; i < newWin + 5; i++) {
      var val = pool[i], lbl = labels[i];
      var btn = document.createElement('button');
      btn.className = 'btn btn-sm ' + (val === window._smCur ? 'btn-primary' : 'btn-outline-secondary');
      btn.dataset.month = val;
      btn.onclick = function() { searchSetMonth(this.dataset.month, this); };
      btn.textContent = lbl;
      nav.appendChild(btn);
    }
    // Hand off to the CSS animation; remove the inline override first so the
    // animation's own opacity values drive the transition cleanly.
    nav.style.opacity = '';
    nav.classList.add(dir > 0 ? 'sm-in-r' : 'sm-in-l');
    nav.addEventListener('animationend', function clean() {
      nav.classList.remove('sm-in-r','sm-in-l');
      nav.removeEventListener('animationend', clean);
    });
    if (prev) prev.disabled = newWin <= 0;
    if (next) next.disabled = newWin + 5 >= pool.length;
  }, 130);
};

window.searchSortByCol = function(col) {
  if (window._searchSortCol === col) {
    window._searchSortDir = window._searchSortDir === 'asc' ? 'desc' : 'asc';
  } else {
    window._searchSortCol = col;
    window._searchSortDir = 'asc';
  }
  document.querySelectorAll('.search-th-sort').forEach(function(th) {
    var arrow = th.querySelector('.search-sort-arrow');
    if (!arrow) return;
    if (th.dataset.col === col) {
      arrow.textContent = window._searchSortDir === 'asc' ? ' ↑' : ' ↓';
      arrow.classList.add('active');
    } else {
      arrow.textContent = ' ↕';
      arrow.classList.remove('active');
    }
  });
  if (window.searchFilterAndSort) window.searchFilterAndSort();
};
