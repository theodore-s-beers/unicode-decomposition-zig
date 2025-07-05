const std = @import("std");

pub fn main() !void {
    //
    // Set up allocator
    //

    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    const allocator = gpa.allocator();

    //
    // Read UnicodeData.txt and normalize newlines
    //

    const contents = try std.fs.cwd().readFileAlloc(allocator, "UnicodeData.txt", 3 * 1024 * 1024);
    const normalized = try std.mem.replaceOwned(u8, allocator, contents, "\r\n", "\n");

    allocator.free(contents);
    defer allocator.free(normalized);

    //
    // Set up decomposition map
    //

    var listed = std.AutoHashMap(u32, []const u32).init(allocator);
    defer {
        var iter = listed.iterator();
        while (iter.next()) |entry| allocator.free(entry.value_ptr.*);
        listed.deinit();
    }

    var canonical = std.AutoHashMap(u32, []const u32).init(allocator);
    defer {
        var map_iter = canonical.iterator();
        while (map_iter.next()) |entry| allocator.free(entry.value_ptr.*);
        canonical.deinit();
    }

    //
    // Iterate over lines and find canonical decompositions
    //

    var line_it = std.mem.splitScalar(u8, normalized, '\n');

    while (line_it.next()) |line| {
        if (line.len == 0) continue;

        var fields = std.ArrayList([]const u8).init(allocator);

        var field_iter = std.mem.splitScalar(u8, line, ';');
        while (field_iter.next()) |field| try fields.append(field);

        const code_point = try std.fmt.parseInt(u32, fields.items[0], 16);

        if ((0x3400 <= code_point and code_point <= 0x4DBF) // CJK ext A
        or (0x4E00 <= code_point and code_point <= 0x9FFF) // CJK
        or (0xAC00 <= code_point and code_point <= 0xD7A3) // Hangul
        or (0xD800 <= code_point and code_point <= 0xDFFF) // Surrogates
        or (0xE000 <= code_point and code_point <= 0xF8FF) // Private use
        or (0x17000 <= code_point and code_point <= 0x187F7) // Tangut
        or (0x18D00 <= code_point and code_point <= 0x18D08) // Tangut suppl
        or (0x20000 <= code_point and code_point <= 0x2A6DF) // CJK ext B
        or (0x2A700 <= code_point and code_point <= 0x2B738) // CJK ext C
        or (0x2B740 <= code_point and code_point <= 0x2B81D) // CJK ext D
        or (0x2B820 <= code_point and code_point <= 0x2CEA1) // CJK ext E
        or (0x2CEB0 <= code_point and code_point <= 0x2EBE0) // CJK ext F
        or (0x30000 <= code_point and code_point <= 0x3134A) // CJK ext G
        or (0xF0000 <= code_point and code_point <= 0xFFFFD) // Plane 15 private use
        or (0x10_0000 <= code_point and code_point <= 0x10_FFFD) // Plane 16 private use
        ) {
            fields.deinit();
            continue;
        }

        const decomp_column = fields.items[5];
        fields.deinit();

        if (decomp_column.len == 0) continue; // No decomposition

        if (std.mem.indexOfScalar(u8, decomp_column, '<')) |_| {
            continue; // Non-canonical decomposition
        }

        var decomps = std.ArrayList(u32).init(allocator);
        defer decomps.deinit();

        var decomp_iter = std.mem.splitScalar(u8, decomp_column, ' ');
        while (decomp_iter.next()) |decomp_str| {
            std.debug.assert(4 <= decomp_str.len and decomp_str.len <= 5);

            const decomp = try std.fmt.parseInt(u32, decomp_str, 16);
            try decomps.append(decomp);
        }

        std.debug.assert(decomps.items.len > 0);

        try listed.put(code_point, try decomps.toOwnedSlice());
    }

    var listed_it = listed.iterator();

    while (listed_it.next()) |kv| {
        const code_point = kv.key_ptr.*;
        const decomps = kv.value_ptr.*;

        const final_decomp: []const u32 = blk: {
            if (decomps.len == 1) {
                // Single-code-point decomposition; recurse simply
                break :blk try getCanonicalDecomp(allocator, &listed, decomps[0]);
            } else {
                // Multi-code-point decomposition; recurse badly
                var result = std.ArrayList(u32).init(allocator);
                defer result.deinit();

                for (decomps) |d| {
                    const c = try getCanonicalDecomp(allocator, &listed, d);
                    defer allocator.free(c);

                    try result.appendSlice(c);
                }

                break :blk try result.toOwnedSlice();
            }
        };

        try canonical.put(code_point, final_decomp);
    }

    //
    // Write decomposition map to JSON for debugging
    //

    const out_json = try std.fs.cwd().createFile("decomp.json", .{ .truncate = true });
    defer out_json.close();

    var ws = std.json.writeStream(out_json.writer(), .{});
    try ws.beginObject();

    var map_iter = canonical.iterator();
    while (map_iter.next()) |entry| {
        const key_str = try std.fmt.allocPrint(allocator, "{}", .{entry.key_ptr.*});
        defer allocator.free(key_str);

        try ws.objectField(key_str);

        try ws.beginArray();
        for (entry.value_ptr.*) |value| try ws.write(value);
        try ws.endArray();
    }

    try ws.endObject();

    //
    // More importantly, save the map in binary format
    //

    try saveDecompMap(&canonical, "decomp.bin");
}

const DecompEntryHeader = packed struct {
    key: u32,
    len: u8,
};

const DecompMapHeader = packed struct {
    count: u32,
    total_bytes: u32,
};

fn getCanonicalDecomp(
    alloc: std.mem.Allocator,
    listed: *const std.AutoHashMap(u32, []const u32),
    code_point: u32,
) ![]const u32 {
    const decomp = listed.get(code_point) orelse {
        const result = try alloc.alloc(u32, 1);
        result[0] = code_point;
        return result;
    };

    // If the decomposition is a single code point, return it directly
    if (decomp.len == 1) {
        const result = try alloc.alloc(u32, 1);
        result[0] = decomp[0];
        return result;
    }

    // Otherwise, we need to recurse for the canonical decomposition
    var result = std.ArrayList(u32).init(alloc);

    for (decomp) |d| {
        const c = try getCanonicalDecomp(alloc, listed, d);
        defer alloc.free(c);

        try result.appendSlice(c);
    }

    return result.toOwnedSlice();
}

fn saveDecompMap(map: *const std.AutoHashMap(u32, []const u32), path: []const u8) !void {
    const file = try std.fs.cwd().createFile(path, .{ .truncate = true });
    defer file.close();

    var bw = std.io.bufferedWriter(file.writer());

    var payload_bytes: u32 = 0;
    var payload_it = map.iterator();
    while (payload_it.next()) |kv| {
        const values = kv.value_ptr.*;
        payload_bytes += @sizeOf(DecompEntryHeader);
        payload_bytes += @intCast(values.len * @sizeOf(u32));
    }

    const main_header = DecompMapHeader{
        .count = std.mem.nativeToLittle(u32, @intCast(map.count())),
        .total_bytes = std.mem.nativeToLittle(u32, payload_bytes),
    };
    try bw.writer().writeStruct(main_header);

    var write_it = map.iterator();
    while (write_it.next()) |kv| {
        const values = kv.value_ptr.*;
        const entry_header = DecompEntryHeader{
            .key = std.mem.nativeToLittle(u32, kv.key_ptr.*),
            .len = @intCast(values.len), // u8 has no endianness
        };

        try bw.writer().writeStruct(entry_header);
        for (values) |v| try bw.writer().writeInt(u32, v, .little);
    }

    try bw.flush();
}
