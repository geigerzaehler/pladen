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

    function artistTrack(d: TrackData): string;
    interface TrackData {
        title: string;
        downloadable: boolean
    }

    function trackRelease(d: TrackReleaseData): string;
    interface TrackReleaseData {
        id: number;
        title: string;
        artist: string;
        year?: string;
        downloadable: boolean
        added?: string;
        addedIso?: string;
    }

    function dragTrack(): DragTrackTemplate;
    interface DragTrackTemplate {
        el: HTMLElement;
        title(s: string);
        artist(s: string);
    }
}
