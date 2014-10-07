/// <reference path="../../../typings/tsd.d.ts" />
import TemplateView = require('./template');
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
class DataTemplateView extends TemplateView {

    /**
     * Return an object that extends the context.
     */
    helper(data: any): any {return {}}

    constructor(public model: any) {
        super();
    }

    /**
     * Return JSON-infied model extended with helpers.
     */
    get context(): {} {
        var data;
        if (this.model && this.model.toJSON)
            data = this.model.toJSON();
        else
            data = this.model;
        return extend({}, data, this.helper(data));
    }
}

export = DataTemplateView
