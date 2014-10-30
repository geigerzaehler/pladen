declare module "bacon" {
    function fromEventTarget(t: EventTarget, name: string): Stream<Event>;

    function update<T>(
        initial: T
      , pat1: Observable<any>[], f1: (...any) => T
    ): Property<T>;
    function update<T>(
        initial: T
      , pat1: Observable<any>[], f1: (...any) => T
      , pat2: Observable<any>[], f2: (...any) => T
    ): Property<T>;
    function update<T>(
        initial: T
      , pat1: Observable<any>[], f1: (...any) => T
      , pat2: Observable<any>[], f2: (...any) => T
      , pat3: Observable<any>[], f3: (...any) => T
    ): Property<T>;
    function update<T>(
        initial: T
      , ...any
    ): Property<T>;


    function when<T>(
        pat1: Observable<any>[], f1: (...any) => T
    ): Stream<T>;
    function when<T>(
        pat1: Observable<any>[], f1: (...any) => T
      , pat2: Observable<any>[], f2: (...any) => T
    ): Stream<T>;
    function when<T>(
        pat1: Observable<any>[], f1: (...any) => T
      , pat2: Observable<any>[], f2: (...any) => T
      , pat3: Observable<any>[], f3: (...any) => T
    ): Stream<T>;

    function combineWith<T>(
        f: (...any) => T
      , ...props: Property<any>[]
    ): Property<T>;


    interface Bus<T> extends Stream<T> {
        push(value: T): void;
        end(): void;
        error(e: Error): void;
        plug(o: Observable<T>);
    }

    var Bus: {
        new<T> (): Bus<T>;
    }

    interface Observable<T> {
        onValue(handler: (t:T) => any);

        map<S>(f: (t:T) => S): Observable<S>;
        filter(f: (t:T) => boolean): Observable<T>;
        scan<S>(seed: S, f: (s:S, t:T) => S): Property<S>;
        fold<S>(seed: S, f: (s:S, t:T) => S): Property<S>;

        doAction(f: (t:T) => void);

        merge(o: Observable<T>): Observable<T>;

        log(msg: string);
    }

    interface Stream<T> extends Observable<T> {
        toProperty(seed?: T): Property<T>;

        map<S>(f: (t:T) => S): Stream<S>;
        filter(f: (t:T) => boolean): Stream<T>;
        merge(o: Stream<T>): Stream<T>;
    }

    interface Property<T> extends Observable<T> {
        assign(o: any, method: string, ...args: any[]);
        toEventStream(): Stream<T>;

        map<S>(f: (t:T) => S): Property<S>;
        filter(f: (t:T) => boolean): Property<T>;
    }
}
