int linear_search(int* arr, int val, int n) {
    for (int i = 0; i < n; i++) {
        if (arr[i] == val) return 1;
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

int cube(int n) {
    int res = 0; 
    for (int i = 0; i < n; i++) {
        for (int j = i * i; j < n * n; j++) {
            res++;
        }
    }
    return res;
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


void matmul( /*target parameter*/ int n, /*free var*/ int m, /* free var */ int l, int* lhs, int* rhs, int* res) {
    for(int i = 0; i < n; i++) {
        for (int j = 0; j < l; j++) {
            res[l * i + j] = 0;
            for (int k = 0; k < m; k++) {
                res[l * i + j] += lhs[m * i + k] * rhs[l * k + j]; 
            }
        }
    }
}
