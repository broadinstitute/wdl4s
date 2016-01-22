package wdl4s.parser;

public enum MemoryUnit {
    Bytes(1, "B"),
    KiB(1 << 10, "KiB"),
    MiB(1 << 20, "MiB"),
    GiB(1 << 30, "GiB"),
    TiB(1 << 40, "TiB"),
    KB(1000, "KB"),
    MB(KB.bytes * 1000, "MB", "M"),
    GB(MB.bytes * 1000, "GB", "G"),
    TB(GB.bytes * 1000, "TB", "T");

    public final double bytes;
    public final String[] suffixes;

    MemoryUnit(double bytes, String... suffixes) {
        this.bytes = bytes;
        this.suffixes = suffixes;
    }
}

