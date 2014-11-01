export = template;
function template(src: string) {
    var p, attrs, wrapper, t: any = {};
    wrapper = document.createElement('div');
    wrapper.innerHTML = src;
    t.el = wrapper.removeChild(wrapper.children[0]);

    walk(t.el, function(e: HTMLElement) {
        if (p = textPlaceholder(e))
            t[p] = textMutator(e);
        
        attrs = e.attributes;
        for(var i=attrs.length-1; i>=0; i--) {
            if (attrs[i].name == 'class')
                continue;
            if (p = placeholder(attrs[i].value))
                t[p] = attrMutator(attrs[i].name, e);
        }

        e.className.split(' ').forEach(function(c) {
            if (p = placeholder(c))
                t[p] = classMutator(p, e);
        })
        
    })
    return t;
}


function walk(r: HTMLElement, iterator: (e: HTMLElement) => void) {
    var n;
    var w = document.createNodeIterator(r, NodeFilter.SHOW_ELEMENT, null, false);
    iterator(r);
    while (n = w.nextNode()) {
        iterator(n);
    }
}


var placeholderRE = /^\s*{{(\w+)}}\s*$/;
function textPlaceholder(e: HTMLElement): string {
    if (e.children.length != 0)
        return null;
    return placeholder(e.textContent)
}

function placeholder(s: string): string {
    var match = placeholderRE.exec(s);
    return match && match[1];
}


function textMutator(n: Node) {
    return function(t: string) { n.textContent = t }
}


function attrMutator(name: string, e: HTMLElement) {
    return function(v: string) { e.setAttribute(name, v); }
}


function classMutator(name: string, e: HTMLElement) {
    var prev;
    return function(v: any) {
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
