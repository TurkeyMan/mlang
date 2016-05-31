#pragma once

struct SourceLocation {
	int Line;
	int Col;
};


extern SourceLocation CurLoc;
extern SourceLocation LexLoc;
