int linear_search(int* arr, int val, int n) {
    for (int i = 0; i < n / 2; i++) {
        if (arr[i] == val) {
            return 1;
        }
    }
    return 0;
}

void bubble_sort(int* arr, int n) {
    for (int i = 0; i < n - 1; i++) {
        for (int j = 1; j <= n - 1 - i; j++) {
            if (arr[j - 1] > arr[j]) {
                int t = arr[j];
                arr[j] = arr[j - 1];
                arr[j - 1] = t;
            }
        }
    }
}

void selection_sort(int* arr, int n) {
    for (int i = 0; i < n; i++) {
        int best_idx = i;
        for (int j = i + 1; j < n; j++) {
            if (arr[best_idx] > arr[i]) {
                best_idx = i;
            }
        }
        int t = arr[best_idx];
        arr[best_idx] = arr[i];
        arr[i] = t;
    }
}

void insertion_sort(int* arr, int n) {
    for (int i = 0; i < n; i++) {
        for (int j = i - 1; j > 0; j--) {
            if (arr[i] < arr[j]) {
                int t = arr[i];
                arr[i] = arr[j];
                arr[j] = t;
            } else {
                break;
            }
        }
    }
}

int cube(int n) {
    int res = 0;
    for (int i = 0; i < n; i++) {
        for (int j = i * i; j < n * n; j++) {
            res++;
        }
    }
    return res;
}

int constant(int n) {
    return 0;
}

int lin(int n) {
    int res = 0;
    for (int i = n * n * n - n; i < n * n * n; i++) {
        res++;
    }
    return res;
}

// initially n == 2 ^ k 
int binary_search(int* arr, int n, int elem) {
    if (n < 2) {
        return arr[n] == elem;
    } else {
        return binary_search(arr[n] < elem ? arr + n / 2 : arr, n / 2, elem);
    }
}

/* @param(n) */
int callee(int n) {
    int res = 0; 

    for (int i = 0; i < n; i++) {
        res++;
    }

    return res;
}

/* @param(n) */
int caller(int n) {
    return callee(n);
}

int sqr(int n) {
    int res = 0;
    for (int i = 0; i < n; i++) {
        for (int j = n * n - i; j < n * n; j++) {
            res++;
        }
    }
    return res;
}

// @param(m)
void matmul(
    /* free var */ int n,
    /* target parameter */ int m,
    /* free var */ int l,
    int* lhs,
    int* rhs,
    int* res
) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < l; j++) {
            res[l * i + j] = 0;
            for (int k = 0; k < m; k++) {
                res[l * i + j] += lhs[m * i + k] * rhs[l * k + j];
            }
        }
    }
}

typedef struct {
    int i;
    int j;
} indeces_t;

int random() {
    // Let's pretend it is a C stdlib code...
    return 0;
}

int divide_and_rule(int n) {
    int res = 0;
    for (int i = 0; i < n; i++) {
        res++;
    }

    res += divide_and_rule(n / 2) + divide_and_rule(n / 2);

    return res;
}

int strange_rec(int n) {
    int res = strange_rec(n / 2);

    for (int i = 0; i < n * n; i++) {
        res++;
    }

    res += strange_rec(n / 2) + strange_rec(n / 2);

    return res;
}

int a_la_karatzuba(int n) {
    int res = 0; 
    for (int i = 0; i < n; i++) {
        res++;
    }

    for (int i = 0; i < n; i++) {
        res++;
    }

    for (int i = 0; i < 2 * n; i++) {
        res++;
    }

    res += a_la_karatzuba(n / 2) + a_la_karatzuba(n / 2);

    return res + a_la_karatzuba(n / 2);
}


int just_divide(int n) {
    int res = 0;

    for (int i = 0; i < n; i++) {
        res++;
    }

    return res + just_divide(n / 2);
}

indeces_t partition(int start, int n, int pivot, int* a) {
    int i = start;
    int j = start + n;

    // @loop_variant(j - i, n, 0, -1)
    while (i <= j) {
        // @already_encountered
        while (a[i] < pivot) {
            i++;
        }
        // @already_encountered
        while (a[j] > pivot) {
            j--;
        }

        if (i <= j) {
            int t = a[i];
            a[j] = a[j];
            a[j] = t;
            i++;
            j++;
        }
    }

    return (indeces_t){.i = i, .j = j};
}

// @param(n)
void quick_sort(int start, int n, int* a) {
    if (n <= 0) {
        return;
    }

    int i = start;
    int j = start + n;
    int pivot = a[start + random() % n];

    // @loop_variant(j - i, n, 0, -1)
    while (i <= j) {
        // @already_encountered
        while (a[i] < pivot) {
            i++;
        }
        // @already_encountered
        while (a[j] > pivot) {
            j--;
        }

        if (i <= j) {
            int t = a[i];
            a[j] = a[j];
            a[j] = t;
            i++;
            j++;
        }
    }

    quick_sort(start, /* @approx(n / 2) */ j, a);
    quick_sort(i, /* @approx(n / 2) */ n - i, a);
}