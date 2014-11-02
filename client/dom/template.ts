export = template;
function template(src: string) {
    var p, attrs, wrapper, t: any = {};
    wrapper = document.createElement('div');
    wrapper.innerHTML = src;
    t.el = wrapper.removeChild(wrapper.children[0]);

    var show = NodeFilter.SHOW_ELEMENT | NodeFilter.SHOW_TEXT;
    var w = document.createTreeWalker(t.el, show, null, false);
    var e = t.el
    while (e) {
        if (e.nodeType == Node.TEXT_NODE) {
            if (p = placeholder(e.textContent))
                t[p] = textMutator(e);
            e = w.nextNode();
            continue;
        }
        if (e.hasAttribute('data-slot')) {
            p = e.getAttribute('data-slot');
            t[p] = slotMutator(e);
            e = w.nextSibling();
            continue
        }
        
        eachAttribute(e, (name, value) => {
            if (name == 'class')
                return;

            if (p = onlyPlaceholder(name)) {
                e.removeAttribute(name);
                t[p] = attrMutator(p, e);
                return
            }

            if (p = placeholder(value))
                t[p] = attrMutator(name, e);
        });

        e.className.split(' ').forEach(function(c) {
            if (p = onlyPlaceholder(c))
                t[p] = classMutator(p, e);
        })
        e = w.nextNode();
    }

    return t;
}


var placeholderId = '[a-zA-Z][-\\w]*'

var placeholderMatchRE = new RegExp('{{(' + placeholderId + ')}}');
function placeholder(s: string): string {
    var match = placeholderMatchRE.exec(s);
    return match && camelCase(match[1]);
}


var onlyPlaceholderMatchRE = new RegExp('^\\s*{{(' + placeholderId + ')}}\\s*$');
function onlyPlaceholder(s: string): string {
    var match = onlyPlaceholderMatchRE.exec(s);
    return match && camelCase(match[1]);
}


var placeholderRE = new RegExp('{{' + placeholderId + '}}');
function compileSinglePlaceholder(src: string) {
    var tokens = src.split(placeholderRE);
    if (tokens.length > 2)
        throw Error('template contains more than one placeholder: ' + src);
    var before = tokens[0];
    var after = tokens[1]
    return function evaluate(v: any) {
        return before + v + after;
    }

}


function textMutator(n: Node) {
    var render = compileSinglePlaceholder(n.textContent)
    return function setText(t: string) { n.textContent = render(t) }
}


function attrMutator(name: string, e: HTMLElement) {
    return function setAttr(v: string) { e.setAttribute(name, v) }
}


function classMutator(name: string, e: HTMLElement) {
    var prev;
    return function setClass(v: any) {
        if (prev)
            e.classList.remove(prev);
        if (v === true) {
            e.classList.add(name);
            prev = name;
        } else if (v === false) {
            e.classList.remove(name);
            prev = false;
        } else if (v) {
            e.classList.add(v);
            prev = v;
        }
    }
}


function slotMutator(e: HTMLElement) {
    var current = e;
    return function replaceElement(r?: HTMLElement) {
        if (!r)
            r = e;
        current.parentElement.replaceChild(r, current);
        current = r;
    }
}


function eachAttribute(e: HTMLElement, iterator: (n: string, v:string) => void) {
    var attrs = e.attributes;
    for(var i=attrs.length-1; i>=0; i--)
        iterator(attrs[i].name, attrs[i].value);
}


function camelCase(s: string) {
    return s.replace(/[-_]+([a-zA-Z])/g, (_, l) => l.toUpperCase());
}
