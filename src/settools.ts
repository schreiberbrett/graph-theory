class SpernerFamily<T> {
    private family: Array<Set<T>> = new Array<Set<T>>();

    public add(elem: Set<T>) {
        for (let x of this.family) {
            if (isSubset(x, elem)) {
                return;
            }
        }

        this.family.push(elem);
    }
}

function isSubset<T>(subset: Set<T>, superset: Set<T>): boolean {
    for (let elem of subset.keys()) {
        if (!superset.has(elem)) {
            return false;
        }
    }

    return true;
}