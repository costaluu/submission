import csv

with open('c++-results.csv', 'r') as infile, open('fixed-c++-results.csv', 'w', newline='') as outfile:
    reader = csv.reader(infile)
    writer = csv.writer(outfile)
    
    # Read and write header
    header = next(reader)
    writer.writerow(header + ['pair_id'])
    
    group_count = 1
    chunk = []
    
    for i, row in enumerate(reader, start=1):  # Start counting from 1 after header
        chunk.append(row)
        
        if i % 3 == 0:  # Every 3rd data row (not counting header)
            pl = chunk[0][0]  # Get PL from first row in group
            pair_id = f"{pl}_{group_count:02d}"
            
            for row_in_chunk in chunk:
                writer.writerow(row_in_chunk + [pair_id])
            
            chunk = []
            group_count += 1