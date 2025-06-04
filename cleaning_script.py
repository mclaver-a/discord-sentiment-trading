import json
import os
import re

def clean_discord_messages(input_dir, output_dir):
    """
    Cleans Discord message data from JSON files in a directory.

    Args:
        input_dir (str): The directory containing the raw JSON files.
        output_dir (str): The directory where cleaned JSON files will be saved.
    """
    if not os.path.exists(input_dir):
        print(f"Error: Input directory '{input_dir}' not found.")
        return

    os.makedirs(output_dir, exist_ok=True)
    print(f"Output will be saved to: {output_dir}")

    # Regex to find files like '..._page_X.json' and extract X
    # This pattern specifically looks for the "⛩丨dojo-chat_page_" prefix.
    filename_pattern = re.compile(r"⛩丨dojo-chat_page_(\d+)\.json$")

    processed_files = 0
    skipped_files = 0

    for filename in os.listdir(input_dir):
        match = filename_pattern.search(filename)
        if match:
            file_number = match.group(1)
            input_file_path = os.path.join(input_dir, filename)
            output_filename = f"Dojo_{file_number}.json"
            output_file_path = os.path.join(output_dir, output_filename)

            print(f"Processing '{input_file_path}'...")

            cleaned_messages_for_file = []
            try:
                with open(input_file_path, 'r', encoding='utf-8') as f_in:
                    messages = json.load(f_in)

                if not isinstance(messages, list):
                    print(f"Warning: Content of '{filename}' is not a list. Skipping.")
                    skipped_files += 1
                    continue

                for msg in messages:
                    if not isinstance(msg, dict):
                        print(f"Warning: Found non-dictionary item in '{filename}'. Skipping item.")
                        continue

                    # Extract username:
                    # Prioritize author.username, fallback to userName (if present), then 'Unknown'
                    author_info = msg.get('author', {})
                    username = 'Unknown' # Default
                    if isinstance(author_info, dict) and author_info.get('username'):
                        username = author_info.get('username')
                    elif msg.get('userName'): # Fallback to top-level userName if author.username is not good
                        username = msg.get('userName')


                    timestamp = msg.get('timestamp', 'N/A')
                    content = msg.get('content', '')

                    # Extract mentions (list of usernames)
                    raw_mentions_list = msg.get('mentions', [])
                    mention_usernames = []
                    if isinstance(raw_mentions_list, list):
                        for mention_obj in raw_mentions_list:
                            if isinstance(mention_obj, dict) and 'username' in mention_obj:
                                mention_usernames.append(mention_obj['username'])
                    
                    cleaned_message = {
                        "Username": username,
                        "timestamp": timestamp,
                        "mentions": mention_usernames,
                        "content": content
                    }
                    cleaned_messages_for_file.append(cleaned_message)

                with open(output_file_path, 'w', encoding='utf-8') as f_out:
                    json.dump(cleaned_messages_for_file, f_out, indent=2, ensure_ascii=False)
                print(f"Successfully processed and saved: '{output_file_path}'")
                processed_files += 1

            except FileNotFoundError:
                print(f"Error: File not found '{input_file_path}' (should not happen if os.listdir worked).")
                skipped_files +=1
            except json.JSONDecodeError:
                print(f"Error: Could not decode JSON from '{input_file_path}'. Skipping.")
                skipped_files +=1
            except Exception as e:
                print(f"An unexpected error occurred with '{input_file_path}': {e}. Skipping.")
                skipped_files +=1
        # else: # Optional: to see which files are skipped due to pattern mismatch
        #     if filename.endswith(".json"): # Only print for JSON files
        #         print(f"Skipping '{filename}' as it doesn't match the naming pattern '⛩丨dojo-chat_page_X.json'.")


    print(f"\nBatch processing complete. Processed {processed_files} files. Skipped {skipped_files} files.")

if __name__ == "__main__":
    # IMPORTANT: Replace this with the actual path to your input directory
    # The directory should contain files like "⛩丨dojo-chat_page_1.json", "⛩丨dojo-chat_page_2.json", etc.
    # The path you provided in the prompt (⛩丨dojo-chat_3a9f687a-0968-4ac6-b2d4-e60869a591f0)
    # does not look like a JSON filename and does not match the "_page_X.json" structure.
    # I'm assuming your files are *inside* the directory '/Users/Marti/Downloads/Dojo_General_chat/'
    # and have names like '⛩丨dojo-chat_page_1.json'.
    
    input_directory = "/Users/Marti/Downloads/Dojo_General_chat/⛩丨dojo-chat_3a9f687a-0968-4ac6-b2d4-e60869a591f0/"
    
    # You can change the output directory name if you like
    output_directory = "/Users/Marti/Downloads/Dojo_General_chat/cleaned_dojo_files" 
    
    # To make it easier to run, let's create a dummy input file based on your example
    # if you are running this for real, you can comment out this dummy file creation part.
    if not os.path.exists(input_directory):
        os.makedirs(input_directory)
        print(f"Created dummy input directory: {input_directory}")

    dummy_file_name = "⛩丨dojo-chat_page_1.json"
    dummy_file_path = os.path.join(input_directory, dummy_file_name)
    
    # Only create dummy if it doesn't exist, to avoid overwriting your actual file
    if not os.path.exists(dummy_file_path):
        dummy_data = [{"id":"1138997136055337040","channel_id":"930597674904870952","author":{"id":"781598106029654046","username":"0xjunglebook","avatar":"15583afe8ca26f36e6d516cfe4ac5446","discriminator":"0","public_flags":0,"flags":0,"banner":None,"accent_color":None,"global_name":"Junglebook","avatar_decoration_data":None,"collectibles":None,"banner_color":None,"clan":None,"primary_guild":None},"content":"annoying that the unit of measurements are not in $ of plsARB or ARB","timestamp":"2023-08-10T00:47:58.779000+00:00","edited_timestamp":"2023-08-10T00:48:04.888000+00:00","tts":False,"mention_everyone":False,"mentions":[],"attachments":[],"embeds":[],"pinned":False,"type":0,"flags":0,"components":[],"userName":"0xjunglebook"},{"id":"1139001348998828083","channel_id":"930597674904870952","author":{"id":"473726175479595018","username":"dknugo","avatar":"f6ee5893ceb528aa8d34cc97b9503cbc","discriminator":"0","public_flags":0,"flags":0,"banner":None,"accent_color":None,"global_name":"dknugo","avatar_decoration_data":None,"collectibles":None,"banner_color":None,"clan":None,"primary_guild":None},"content":"","timestamp":"2023-08-10T01:04:43.223000+00:00","edited_timestamp":None,"tts":False,"mention_everyone":False,"mentions":[{"id":"781598106029654046","username":"0xjunglebook","avatar":"15583afe8ca26f36e6d516cfe4ac5446","discriminator":"0","public_flags":0,"flags":0,"banner":None,"accent_color":None,"global_name":"Junglebook","avatar_decoration_data":None,"collectibles":None,"banner_color":None,"clan":None,"primary_guild":None}],"attachments":[{"id":"1139001348742983750","filename":"image.png","size":170379,"url":"https://cdn.discordapp.com/attachments/930597674904870952/1139001348742983750/image.png?ex=6816bd2b&is=68156bab&hm=dae0044915dfb23b39a26f8b6067e4ad94f091ff484de43c3e93be9e375bf377&","proxy_url":"https://media.discordapp.net/attachments/930597674904870952/1139001348742983750/image.png?ex=6816bd2b&is=68156bab&hm=dae0044915dfb23b39a26f8b6067e4ad94f091ff484de43c3e93be9e375bf377&","width":2038,"height":1018,"content_type":"image/png"}],"embeds":[],"reactions":[{"emoji":{"id":None,"name":"👍"},"count":2,"count_details":{"burst":0,"normal":2},"burst_colors":[],"me_burst":False,"burst_me":False,"me":False,"burst_count":0}],"pinned":False,"type":19,"message_reference":{"type":0,"channel_id":"930597674904870952","message_id":"1138997102026964992","guild_id":"903450941347467286"},"flags":0,"referenced_message":{"type":0,"content":"not that many","mentions":[],"mention_roles":[],"attachments":[],"embeds":[],"timestamp":"2023-08-10T00:47:50.666000+00:00","edited_timestamp":None,"flags":0,"components":[],"id":"1138997102026964992","channel_id":"930597674904870952","author":{"id":"781598106029654046","username":"0xjunglebook","avatar":"15583afe8ca26f36e6d516cfe4ac5446","discriminator":"0","public_flags":0,"flags":0,"banner":None,"accent_color":None,"global_name":"Junglebook","avatar_decoration_data":None,"collectibles":None,"banner_color":None,"clan":None,"primary_guild":None},"pinned":False,"mention_everyone":False,"tts":False},"components":[],"position":0,"userName":"dknugo"}]
        with open(dummy_file_path, 'w', encoding='utf-8') as f_dummy:
            json.dump(dummy_data, f_dummy, indent=2)
        print(f"Created dummy input file for testing: {dummy_file_path}")

    clean_discord_messages(input_directory, output_directory)

