/* Suppose a sorted array is rotated at some pivot unknown to you
   beforehand.

   (i.e., 0 1 2 4 5 6 7 might become 4 5 6 7 0 1 2).

   You are given a target value to search. If found in the array
   return its index, otherwise return -1.

   You may assume no duplicate exists in the array.

*/

#include <iostream>
#include <vector>
#include <iterator>             // std::distance
#include <algorithm>            // std::unique

using namespace std;

/*Bounds and floor-division:
  - Given low and high, where high is exclusive
  - (l + h) / 2
 */

int bisearch(const vector<int>& nums, int target) {
    int low = 0, high = nums.size();
    while (low != high) {
        const int mid = low + (high - low) / 2;
        /*
          int D = high - low
          if (D == 2*d)
            assert(mid == low + d) 
            assert(leftRange == [low:mid]) length: d
            assert(rightRange == [mid+1:high]) length: d-1
          else if (D == 2*d + 1)
            assert(mid == low + d) 
            assert(leftRange == [low:mid]) length: d
            assert(rightRange == [mid+1:high]) length: d
          if (high == low + 1)
            assert(low == mid)
            assert(mid+1 == high)
          if (nums[mid] < target)
            low = mid + 1;
        */
        else if (nums[mid] > target)
            high = mid;
        else
            return mid;
    }
    return -1;
}


int search(const vector<int>& nums, int target) {
    
    return -1;
}

int main()
{
    vector<int> nums = {0, 1, 5, 7, 12, 18, 20};
    int k = bisearch(nums, 5);
    cout << k << ": " << nums[k] << endl;
    k = bisearch(nums, 18);
    cout << k << ": " << nums[k] << endl;
    return 0;
}
