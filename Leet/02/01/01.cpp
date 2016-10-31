/* RemoveRemove Duplicates from Sorted Array

Given a sorted array, remove the duplicates in place such that each
element appear only once and return the new length.

*/

#include <vector>
#include <iterator>             // std::distance
#include <algorithm>            // std::unique

using namespace std;

int removeDuplicates(vector<int>& nums){
    if (nums.empty())
        return 0;
    unsigned int index = 0;
    for (unsigned int i = 1; i < nums.size(); i++) {
        if (nums[i] != nums[index]) {
            nums[index] = nums[i];
            index++;
        }
    }
    return index;
}

int remDup(vector<int>& nums) {
    return distance(nums.begin(), unique(nums.begin(), nums.end()));
}
