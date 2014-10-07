/// <reference path="../../../typings/tsd.d.ts" />
import View = require('./view');
import underscore = require('underscore');
import Mustache = require('mustache');
var extend = underscore.extend;

/**
 * TODO fixup doc
 * A view that uses a template to populate its element.
 *
 * A TemplateView is parameterized by data to render and the
 * template, which is retrieved from the DOM. The 'render'
 * method uses these parameters to generate an HTML string for
 * the elements content.
 */
class TemplateView extends View {

    /**
     * The name attribute of the template in the DOM.
     *
     * The actual template is the content of the element selected
     * by "template[name=my-template-name]".
     */
    get template(): string { return "" }

    constructor() {
        super();
        // FIXME should call this
        // this.render();
        // this.delegateEvents();
    }

    /**
     * Generate a string from the 'template' and 'context' and put
     * it into the element.
     */
    render() {
        this.unbindUIElements();

        var template = $("template[name='" + this.template + "']").html();
        var content = Mustache.render(template, this.context);
        this.$el.html(content);

        this.bindUIElements();
    }

    /**
     * Return the context to render the template in.
     */
    get context(): {} {
        return {};
    }
}

export = TemplateView
