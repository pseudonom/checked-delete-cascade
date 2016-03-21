# checked-delete-cascade
Require cascading during deletion via types

Status: In limbo. I realized that `persistent`'s built in delete cascade mechanism only works for grouped entity declarations. i.e. Those that are all in one `share [...|...|]` block. This makes a straightforward extension much less appealing.
