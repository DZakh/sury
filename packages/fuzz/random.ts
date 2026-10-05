export class Random {
  readonly initialSeed: number;
  #state: number;

  constructor(seed: number) {
    this.initialSeed = seed | 0;
    this.#state = this.initialSeed;
  }

  next(): number {
    this.#state = (this.#state + 0x6d2b79f5) | 0;
    let value = this.#state;
    value = Math.imul(value ^ (value >>> 15), value | 1);
    value ^= value + Math.imul(value ^ (value >>> 7), value | 61);
    return ((value ^ (value >>> 14)) >>> 0) / 4294967296;
  }

  int(min: number, max: number): number {
    return min + Math.floor(this.next() * (max - min + 1));
  }

  bool(chance = 0.5): boolean {
    return this.next() < chance;
  }

  pick<T>(values: readonly T[]): T {
    const value = values[Math.floor(this.next() * values.length)];
    if (value === undefined) throw new Error("Cannot pick from an empty list");
    return value;
  }

  weighted<T>(values: readonly { value: T; weight: number }[]): T {
    const total = values.reduce((sum, item) => sum + item.weight, 0);
    let cursor = this.next() * total;
    for (const item of values) {
      cursor -= item.weight;
      if (cursor < 0) return item.value;
    }
    return values[values.length - 1]!.value;
  }
}
