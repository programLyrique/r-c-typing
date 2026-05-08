/* Two .c files in this package both declare a file-scope global named
 * "scan" with different struct types. Each defines a setter that writes
 * to its own "scan", which only types if the runner treats the duplicate
 * Definition global as a per-file internal binding. */

struct word_scan { int word_count; };
struct word_scan scan;

void set_word_count(int n) {
  scan.word_count = n;
}
