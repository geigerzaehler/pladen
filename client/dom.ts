/// <reference path="..//typings/bacon.d.ts" />
import Bacon = require('bacon')


/**
 * This module includes various utitilies the simplify interaction with
 * the DOM.
 */


/**
 * Return a pair of observables that describe the state of a
 * drag-and-drop interaction on the element
 *
 * The function registers drag-and-drop event listeners on the element.
 * It is only interested in DragEvents with `type` in the list of data
 * transfer types. All other events are ignored
 *
 * If a 'dragenter' event with the correct type occurs on `el`, the `over`
 * property is set to `true`. Conversely, if a `dragleave` event occurs
 * it is set to `false`.
 *
 * If the 'drop' event is emitted, the value corresponding to `type` is
 * retrieved from the event's DataTransfer object and send to the
 * `drop` stream. The function prevents the default event handler for
 * drag events with matching type and thus enables the drop.
 */
export function dragDropStream(el: HTMLElement, type: string): DragDropStream {
    function eventFilter(ev: DragEvent) {
        return ev.dataTransfer.types.contains(type);
    }

    function filteredEventStream(event: string) {
        return Bacon.fromEventTarget(el, event).filter(eventFilter);
    }

    var dragenter = filteredEventStream('dragenter');
    var dragleave = filteredEventStream('dragleave');
    var dragover = filteredEventStream('dragover')
                   .doAction((ev: DragEvent) => ev.preventDefault());
    var drop = filteredEventStream('drop')
               .map((ev: DragEvent) => ev.dataTransfer.getData(type));

    var over = Bacon.update(
        false,
        [dragenter], () => true,
        [dragleave], () => false,
        [drop], () => false
    );

    return {drop: drop, over: over};
}

export interface DragDropStream {
    over: Bacon.Property<boolean>;
    drop: Bacon.Stream<string>;
}
