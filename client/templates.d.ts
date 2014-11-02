declare module "templates" {
    var search: string;
    function player(): string;
    function trackContextMenu(): string;

    function artistAlbum(d: AlbumData): string;
    interface AlbumData {
        name: string;
        year?: number;
        downloadable?: boolean
    }

    function dragTrack(): DragTrackTemplate;
    interface DragTrackTemplate {
        el: HTMLElement;
        title(s: string);
        artist(s: string);
    }
}
