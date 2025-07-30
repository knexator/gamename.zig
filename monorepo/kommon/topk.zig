/// feed it with samples of type T, it keeps the 'n' ones with highest '.score'
pub fn TopK(T: type, n: usize) type {
    assert(@hasField(T, "score"));
    assert(@FieldType(T, "score") == f32);
    return struct {
        seen: usize = 0,
        best_samples: [n]T = undefined,
        best_indices: [n]usize = undefined,

        pub fn best(self: @This()) T {
            assert(self.seen > 0);
            return self.best_samples[0];
        }

        pub fn add(self: *@This(), sample: T) void {
            const cur_index = self.seen;
            for (0..@min(self.seen, n)) |k| {
                if (sample.score > self.best_samples[k].score) {
                    std.mem.copyBackwards(T, self.best_samples[k + 1 ..], self.best_samples[k .. n - 1]);
                    self.best_samples[k] = sample;
                    self.best_indices[k] = cur_index;
                    break;
                }
            } else if (self.seen < n) {
                self.best_samples[self.seen] = sample;
                self.best_indices[self.seen] = cur_index;
            }
            self.seen += 1;
        }
    };
}

const std = @import("std");
const assert = std.debug.assert;
