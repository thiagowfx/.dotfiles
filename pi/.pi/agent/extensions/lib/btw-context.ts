interface ContextEntrySource<TEntry> {
	buildContextEntries(): TEntry[];
}

export function contextMessagesFrom<TEntry, TMessage>(
	source: ContextEntrySource<TEntry>,
	project: (entry: TEntry) => TMessage[],
): TMessage[] {
	return source.buildContextEntries().flatMap(project);
}
