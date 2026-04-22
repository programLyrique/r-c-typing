struct node {
  int value;
  struct node *next;
};

int head(struct node *n) {
  return n->value;
}

struct node *tail(struct node *n) {
  return n->next;
}

struct tree {
  int value;
  struct tree *left;
  struct tree *right;
};

int root(struct tree *t) {
  return t->value;
}
