/*----------------------------------------------------------------
 * File        : btree.c
 * Author      : Mikael Pettersson <mikael.pettersson@klarna.com>
 * Description : C implementation of B-tree sets
 *
 * Copyright (c) 2016 Klarna AB
 *
 * This file is provided to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License.  You may obtain
 * a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *----------------------------------------------------------------
 * Straight-forward translation to C of the Pascal code for B-trees in:
 * Niklaus Wirth, "Algorithms + Data Structures = Programs", Program 4.7,
 * pp 252--257, Prentice-Hall, 1976.
 *----------------------------------------------------------------
 */
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

/* B-tree search, insertion, and deletion */

#define N 2
#define NN (2*N)

struct page;
typedef struct page *ref;

typedef struct item {
    int key;
    ref p;
    int count;
} item;

typedef struct page {
    int m; /* 0..NN */ /* no. of items */
    ref p0;
    item e[NN + 1]; /* [1..NN] */
} page;

static page *new_page(void)
{
    page *p = malloc(sizeof(*p));
    if (p == NULL) {
	perror("failed to allocate a new page");
	exit(1);
    }
    return p;
}

/* subroutine of search; insert u to the right of a->e[r] */
static void insert(ref a, int r, item u, bool *h, item *v)
{
    int i;
    ref b;

    if (a->m < NN) {
	a->m += 1;
	*h = false;
	for (i = a->m; i >= r + 2; --i) {
	    a->e[i] = a->e[i - 1];
	}
	a->e[r + 1] = u;
    } else {
	/* page *a is full; split it and assign the emerging
	   item to *v */
	b = new_page();
	if (r <= N) {
	    if (r == N) {
		*v = u;
	    } else {
		*v = a->e[N];
		for (i = N; i >= r + 2; --i) {
		    a->e[i] = a->e[i - 1];
		}
		a->e[r + 1] = u;
	    }
	    for (i = 1; i <= N; ++i) {
		b->e[i] = a->e[i + N];
	    }
	} else {
	    /* insert u in right page */
	    r = r - N;
	    *v = a->e[N + 1];
	    for (i = 1; i <= r - 1; ++i) {
		b->e[i] = a->e[i + N + 1];
	    }
	    b->e[r] = u;
	    for (i = r + 1; i <= N; ++i) {
		b->e[i] = a->e[i + N];
	    }
	}
	a->m = N;
	b->m = N;
	b->p0 = v->p;
	v->p = b;
    }
}

/* Search key x on B-tree with root a; if found, increment counter,
   otherwise insert an item with key x and count 1 in tree. If an item
   emerges to be passed to a lower level, then assign it to *v.
   *h = "tree a has become higher". */
void search(int x, ref a, bool *h, item *v)
{
    int k, l, r;
    ref q;
    item u;

    /* search key x on page *a; *h = false */

    if (a == NULL) {
	/* item with key x is not in tree */
	*h = true;
	v->key = x;
	v->count = 1;
	v->p = NULL;
    } else {
	l = 1;
	r = a->m;
	/* binary array search */
	do {
	    k = (l + r) / 2;
	    if (x <= a->e[k].key) {
		r = k - 1;
	    }
	    if (x >= a->e[k].key) {
		l = k + 1;
	    }
	} while (r >= l);
	if (l - r > 1) {
	    /* found */
	    a->e[k].count += 1;
	    *h = false;
	} else {
	    /* item is not on this page */
	    if (r == 0) {
		q = a->p0;
	    } else {
		q = a->e[r].p;
	    }
	    search(x, q, h, &u);
	    if (*h) {
		insert(a, r, u, h, v);
	    }
	}
    }
}

/* subroutine of delete; a = underflow page, c = ancestor page */
static void underflow(ref c, ref a, int s, bool *h)
{
    ref b;
    int i, k, mb, mc;

    mc = c->m;
    /* *h = true, a->m = N - 1 */
    if (s < mc) {
	/* b = page to the right of a */
	s += 1;
	b = c->e[s].p;
	mb = b->m;
	k = (mb - N + 1) / 2;
	/* k = no. of items available on adjacent page b */
	a->e[N] = c->e[s];
	a->e[N].p = b->p0;
	if (k > 0) {
	    /* move k items from b to a */
	    for (i = 1; i <= k - 1; ++i) {
		a->e[i + N] = b->e[i];
	    }
	    c->e[s] = b->e[k];
	    c->e[s].p = b;
	    b->p0 = b->e[k].p;
	    mb -= k;
	    for (i = 1; i <= mb; ++i) {
		b->e[i] = b->e[i + k];
	    }
	    b->m = mb;
	    a->m = N - 1 + k;
	    *h = false;
	} else {
	    /* merge pages a and b */
	    for (i = 1; i <= N; ++i) {
		a->e[i + N] = b->e[i];
	    }
	    for (i = s; i <= mc - 1; ++i) {
		c->e[i] = c->e[i + 1];
	    }
	    a->m = NN;
	    c->m = mc - 1;
	    /* dispose(b) */
	    *h = c->m < N;
	}
    } else {
	/* b = page to the left of a */
	if (s == 1) {
	    b = c->p0;
	} else {
	    b = c->e[s - 1].p;
	}
	mb = b->m + 1;
	k = (mb - N) / 2;
	if (k > 0) {
	    /* move k items from page b to a */
	    for (i = N - 1; i >= 1; --i) {
		a->e[i + k] = a->e[i];
	    }
	    a->e[k] = c->e[s];
	    a->e[k].p = a->p0;
	    mb -= k;
	    for (i = k - 1; i >= 1; --i) {
		a->e[i] = b->e[i + mb];
	    }
	    a->p0 = b->e[mb].p;
	    c->e[s] = b->e[mb];
	    c->e[s].p = a;
	    b->m = mb - 1;
	    a->m = N - 1 + k;
	    *h = false;
	} else {
	    /* merge pages a and b */
	    b->e[mb] = c->e[s];
	    b->e[mb].p = a->p0;
	    for (i = 1; i <= N - 1; ++i) {
		b->e[i + mb] = a->e[i];
	    }
	    b->m = NN;
	    c->m = mc - 1;
	    /* dispose(a) */
	    *h = c->m < N;
	}
    }
}

/* subroutine of delete */
static void del(ref p, bool *h, ref a, int k)
{
    ref q;
    /* global a, k */

    q = p->e[p->m].p;
    if (q != NULL) {
	del(q, h, a, k);
	if (*h) {
	    underflow(p, q, p->m, h);
	}
    } else {
	p->e[p->m].p = a->e[k].p;
	a->e[k] = p->e[p->m];
	p->m -= 1;
	*h = p->m < N;
    }
}

/* search and delete key x in b-tree a; if a page underflow is
   necessary, balance with adjacent page if possible, otherwise merge;
   *h = "page a is undersize" */
void delete(int x, ref a, bool *h)
{
    int i, k, l, r;
    ref q;

    if (a == NULL) {
	printf("KEY IS NOT IN TREE\n");
	*h = false;
    } else {
	l = 1;
	r = a->m;
	/* binary array search */
	do {
	    k = (l + r) / 2;
	    if (x <= a->e[k].key) {
		r = k - 1;
	    }
	    if (x >= a->e[k].key) {
		l = k + 1;
	    }
	} while (l <= r);
	if (r == 0) {
	    q = a->p0;
	} else {
	    q = a->e[r].p;
	}
	if (l - r > 1) {
	    /* found, now delete a->e[k] */
	    if (q == NULL) {
		/* a is a terminal page */
		a->m -= 1;
		*h = a->m < N;
		for (i = k; i <= a->m; ++i) {
		    a->e[i] = a->e[i + 1];
		}
	    } else {
		del(q, h, a, k);
		if (*h) {
		    underflow(a, q, r, h);
		}
	    }
	} else {
	    delete(x, q, h);
	    if (*h) {
		underflow(a, q, r, h);
	    }
	}
    }
}

void printtree(ref p, int l)
{
    int i;

    if (p != NULL) {
	for (i = 1; i <= l; ++i) {
	    printf("     ");
	}
	for (i = 1; i <= p->m; ++i) {
	    printf(" %4d", p->e[i].key);
	}
	printf("\n");
	printtree(p->p0, l + 1);
	for (i = 1; i <= p->m; ++i) {
	    printtree(p->e[i].p, l + 1);
	}
    }
}

static int read_int(void)
{
    int x;

    if (scanf("%d", &x) != 1)
	return 0;
    return x;
}

void Btree(void)
{
    ref root, q;
    int x;
    bool h;
    item u;

    root = NULL;
    x = read_int();
    while (x != 0) {
	printf("SEARCH KEY %d\n", x);
	search(x, root, &h, &u);
	if (h) {
	    /* insert new base page */
	    q = root;
	    root = new_page();
	    root->m = 1;
	    root->p0 = q;
	    root->e[1] = u;
	}
	printtree(root, 1);
	x = read_int();
    }
    x = read_int();
    while (x != 0) {
	printf("DELETE KEY %d\n", x);
	delete(x, root, &h);
	if (h) {
	    /* base page size was reduced */
	    if (root->m == 0) {
		q = root;
		root = q->p0;
		/* dispose(q) */
	    }
	}
	printtree(root, 1);
	x = read_int();
    }
}

int main(void)
{
    Btree();
    return 0;
}
