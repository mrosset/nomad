// hints.js
// Copyright (C) 2017-2018 Michael Rosset <mike.rosset@gmail.com>

// This file is part of Nomad

// Nomad is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the
// Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// Nomad is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//     See the GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this program.  If not, see <http://www.gnu.org/licenses/>.

/* based on chromium plugin code, adapted by Nibble<.gs@gmail.com> */
var hint_num_str = '';
var hint_elems = [];
var hint_open_in_new_tab = false;
var hint_enabled = false;
function hintMode(newtab){
        hint_enabled = true;
        if (newtab) {
                hint_open_in_new_tab = true;
        } else {
                hint_open_in_new_tab = false;
        }
        setHints();
        document.removeEventListener('keydown', initKeyBind, false);
        document.addEventListener('keydown', hintHandler, false);
        hint_num_str = '';
}
function hintHandler(e){
        e.preventDefault();  //Stop Default Event
        var pressedKey = get_key(e);
        if (pressedKey == 'Enter') {
                if (hint_num_str == '')
                        hint_num_str = '1';
                judgeHintNum(Number(hint_num_str));
        } else if (/[0-9]/.test(pressedKey) == false) {
                removeHints();
        } else {
                hint_num_str += pressedKey;
                var hint_num = Number(hint_num_str);
                if (hint_num * 10 > hint_elems.length + 1) {
                        judgeHintNum(hint_num);
                } else {
                        var hint_elem = hint_elems[hint_num - 1];
                        if (hint_elem != undefined && hint_elem.tagName.toLowerCase() == 'a') {
                                setHighlight(hint_elem, true);
                        }
                }
        }
}
function setHighlight(elem, is_active) {
        if (is_active) {
                var active_elem = document.body.querySelector('a[highlight=hint_active]');
                if (active_elem != undefined)
                        active_elem.setAttribute('highlight', 'hint_elem');
                elem.setAttribute('highlight', 'hint_active');
        } else {
                elem.setAttribute('highlight', 'hint_elem');
        }
}
function setHintRules() {
        if (document.styleSheets.length < 1) {
                var style = document.createElement("style");
                style.appendChild(document.createTextNode(""));
                document.head.appendChild(style);
        }
        var ss = document.styleSheets[0];
        ss.insertRule('a[highlight=hint_elem] {background-color: yellow}', 0);
        ss.insertRule('a[highlight=hint_active] {background-color: lime}', 0);
}
function deleteHintRules() {
        var ss = document.styleSheets[0];
        ss.deleteRule(0);
        ss.deleteRule(0);
}
function judgeHintNum(hint_num) {
        var hint_elem = hint_elems[hint_num - 1];
        if (hint_elem != undefined) {
                execSelect(hint_elem);
        } else {
                removeHints();
        }
}
function execSelect(elem) {
        var tag_name = elem.tagName.toLowerCase();
        var type = elem.type ? elem.type.toLowerCase() : "";
        if (tag_name == 'a' && elem.href != '') {
                setHighlight(elem, true);
                // TODO: ajax, <select>
                if (hint_open_in_new_tab)
                        window.open(elem.href);
                else location.href=elem.href;
        } else if (tag_name == 'input' && (type == "submit" || type == "button" || type == "reset")) {
                elem.click();
        } else if (tag_name == 'input' && (type == "radio" || type == "checkbox")) {
                // TODO: toggle checkbox
                elem.checked = !elem.checked;
        } else if (tag_name == 'input' || tag_name == 'textarea') {
                elem.focus();
                elem.setSelectionRange(elem.value.length, elem.value.length);
        }
        removeHints();
}
function setHints() {
        setHintRules();
        var win_top = window.scrollY;
        var win_bottom = win_top + window.innerHeight;
        var win_left = window.scrollX;
        var win_right = win_left + window.innerWidth;
        // TODO: <area>
        var elems = document.body.querySelectorAll('a, input:not([type=hidden]), textarea, select, button');
        var div = document.createElement('div');
        div.setAttribute('highlight', 'hints');
        document.body.appendChild(div);
        for (var i = 0; i < elems.length; i++) {
                var elem = elems[i];
                if (!isHintDisplay(elem))
                        continue;
                var pos = elem.getBoundingClientRect();
                var elem_top = win_top + pos.top;
                var elem_bottom = win_top + pos.bottom;
                var elem_left = win_left + pos.left;
                var elem_right = win_left + pos.left;
                if ( elem_bottom >= win_top && elem_top <= win_bottom) {
                        hint_elems.push(elem);
                        setHighlight(elem, false);
                        var span = document.createElement('span');
                        span.style.cssText = [
                                'left: ', elem_left, 'px;',
                                'top: ', elem_top, 'px;',
                                'position: absolute;',
                                'font-size: 13px;',
                                'background-color: ' + (hint_open_in_new_tab ? '#ff6600' : 'red') + ';',
                                'color: white;',
                                'font-weight: bold;',
                                'padding: 0px 1px;',
                                'z-index: 100000;'
                        ].join('');
                        span.innerHTML = hint_elems.length;
                        div.appendChild(span);
                        if (elem.tagName.toLowerCase() == 'a') {
                                if (hint_elems.length == 1) {
                                        setHighlight(elem, true);
                                } else {
                                        setHighlight(elem, false);
                                }
                        }
                }
        }
}
function isHintDisplay(elem) {
        var pos = elem.getBoundingClientRect();
        return (pos.height != 0 && pos.width != 0);
}
function removeHints() {
        if (!hint_enabled)
                return;
        hint_enabled = false;
        deleteHintRules();
        for (var i = 0; i < hint_elems.length; i++) {
                hint_elems[i].removeAttribute('highlight');
        }
        hint_elems = [];
        hint_num_str = '';
        var div = document.body.querySelector('div[highlight=hints]');
        if (div != undefined) {
                document.body.removeChild(div);
        }
        document.removeEventListener('keydown', hintHandler, false);
        document.addEventListener('keydown', initKeyBind, false);
}
function addKeyBind( key, func, eve ){
        var pressedKey = get_key(eve);
        if( pressedKey == key ){
                eve.preventDefault();  //Stop Default Event
                eval(func);
        }
}
document.addEventListener( 'keydown', initKeyBind, false );
function initKeyBind(e){
        var t = e.target;
        if( t.nodeType == 1){
                addKeyBind( 'C-f', 'hintMode()', e );
                addKeyBind( 'C-F', 'hintMode(true)', e );
                addKeyBind( 'C-c', 'removeHints()', e );
        }
}

function get_key(evt){
        var key = String.fromCharCode(evt.keyCode)
            ctrl = evt.ctrlKey ? 'C-' : '',
            meta = (evt.metaKey || evt.altKey) ? 'M-' : '',
        shift = evt.shiftKey ? 'S-' : '';
        console.log("key", key)
        if (evt.shiftKey){
                if (/^[a-z]$/.test(key))
                        return ctrl+meta+key.toUpperCase();
                if (/^[0-9]$/.test(key)) {
                        switch(key) {
                                // TODO
                        case "4":
                                key = "$";
                                break;
                        };
                        return key;
                }
                if (/^(Enter|Space|BackSpace|Tab|Esc|Home|End|Left|Right|Up|Down|PageUp|PageDown|F(\d\d?))$/.test(key))
                        return ctrl+meta+shift+key;
        }
        return ctrl+meta+key;
}
