declare module "templates" {
    function search(): string;
    function player(): string;
    function trackContextMenu(): string;
    function dragTrack(): DragTrackTemplate;
    interface DragTrackTemplate {
        el: HTMLElement;
        title(s: string);
        artist(s: string);
    }
}

