#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <queue>
#include <stack>
#include <cassert>

#include "StreamToActionTranslator.h"
#include "../tests/test_utils.h"
int main()
{
	sds::Translator translator(GetTestDriverMappings(32, 1));
	sds::OvertakingFilter<> filter{ translator };
}