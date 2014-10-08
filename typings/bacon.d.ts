declare module "bacon" {
    class Bus<T> implements Observable<T> {
        push(value: T): void;
        end(): void;
        error(e: Error): void;
        plug(o: Observable<T>);

        // Observable
        onValue(handler: (t:T) => any);
        map<S>(f: (t:T) => S): Observable<S>;
        scan<S>(seed: S, f: (s:S, t:T) => S): Property<S>;
        fold<S>(seed: S, f: (s:S, t:T) => S): Property<S>;
        merge(o: Observable<T>): Observable<T>;
        log(msg: string);

        // Stream
        toProperty(seed?: T): Property<T>;
    }

    interface Observable<T> {
        onValue(handler: (t:T) => any);

        map<S>(f: (t:T) => S): Observable<S>;
        scan<S>(seed: S, f: (s:S, t:T) => S): Property<S>;
        fold<S>(seed: S, f: (s:S, t:T) => S): Property<S>;

        merge(o: Observable<T>): Observable<T>;

        log(msg: string);
    }

    interface Stream<T> extends Observable<T> {
        toProperty(seed?: T): Property<T>;

        map<S>(f: (t:T) => S): Stream<S>;
        merge(o: Stream<T>): Stream<T>;
    }

    interface Property<T> extends Observable<T> {
        assign(o: any, method: string, ...args: any[]);
    }
}
