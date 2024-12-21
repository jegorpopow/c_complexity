int linear_search(int* arr, int val, int n) {
    for (int i = 0; i < n; i++) {
        if (arr[i] == val) return 1;
    }
    return 0;
} 

void bubble_sort(int* arr, int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (arr[j - 1] > arr[j]) {
                int t = arr[j];
                arr[j] = arr[j - 1];
                arr[j - 1] = t;
            }
        }
    }
}
