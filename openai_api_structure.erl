%% openai_api_structure.erl
%% Module to define the structure of OpenAI API endpoints
-module(openai_api_structure).

-export([
    get_api_groups/0,
    get_endpoints/1
]).

%% Define the main API groups/categories
-spec get_api_groups() -> [atom()].
get_api_groups() ->
    [
        models,
        completions,
        chat,
        embeddings,
        files,
        fine_tuning,
        images,
        audio,
        moderations,
        assistants,
        threads,
        messages,
        runs,
        batch
    ].

%% Return endpoints for a specific group
-spec get_endpoints(atom()) -> #{atom() => map()}.

%% Models endpoints
get_endpoints(models) ->
    #{
        list_models => #{
            method => get,
            path => "/models",
            description => "Lists the currently available models.",
            required_params => [],
            optional_params => []
        },
        retrieve_model => #{
            method => get,
            path => "/models/:model",
            description => "Retrieves a model instance.",
            required_params => [model],
            optional_params => []
        },
        delete_model => #{
            method => delete,
            path => "/models/:model",
            description => "Delete a fine-tuned model.",
            required_params => [model],
            optional_params => []
        }
    };

%% Completions endpoints
get_endpoints(completions) ->
    #{
        create_completion => #{
            method => post,
            path => "/completions",
            description => "Creates a completion for the provided prompt and parameters.",
            required_params => [model],
            optional_params => [
                prompt, suffix, max_tokens, temperature, top_p, n, 
                stream, logprobs, echo, stop, presence_penalty,
                frequency_penalty, best_of, logit_bias, user
            ]
        }
    };

%% Chat endpoints
get_endpoints(chat) ->
    #{
        create_chat_completion => #{
            method => post,
            path => "/chat/completions",
            description => "Creates a chat completion for the provided messages and parameters.",
            required_params => [model, messages],
            optional_params => [
                temperature, top_p, n, stream, stop, max_tokens,
                presence_penalty, frequency_penalty, logit_bias,
                user, function_call, functions, tools, tool_choice,
                response_format, seed
            ]
        }
    };

%% Embeddings endpoints
get_endpoints(embeddings) ->
    #{
        create_embedding => #{
            method => post,
            path => "/embeddings",
            description => "Creates an embedding vector representing the input text.",
            required_params => [model, input],
            optional_params => [
                user, encoding_format, dimensions
            ]
        }
    };

%% Files endpoints
get_endpoints(files) ->
    #{
        list_files => #{
            method => get,
            path => "/files",
            description => "Returns a list of files that belong to the user's organization.",
            required_params => [],
            optional_params => [purpose]
        },
        upload_file => #{
            method => post,
            path => "/files",
            description => "Upload a file that can be used across various endpoints.",
            required_params => [file, purpose],
            optional_params => []
        },
        delete_file => #{
            method => delete,
            path => "/files/:file_id",
            description => "Delete a file.",
            required_params => [file_id],
            optional_params => []
        },
        retrieve_file => #{
            method => get,
            path => "/files/:file_id",
            description => "Returns information about a specific file.",
            required_params => [file_id],
            optional_params => []
        },
        retrieve_file_content => #{
            method => get,
            path => "/files/:file_id/content",
            description => "Returns the contents of the specified file.",
            required_params => [file_id],
            optional_params => []
        }
    };

%% Fine-tuning endpoints
get_endpoints(fine_tuning) ->
    #{
        create_fine_tuning_job => #{
            method => post,
            path => "/fine_tuning/jobs",
            description => "Creates a job that fine-tunes a specified model.",
            required_params => [training_file, model],
            optional_params => [
                validation_file, hyperparameters, suffix
            ]
        },
        list_fine_tuning_jobs => #{
            method => get,
            path => "/fine_tuning/jobs",
            description => "List your organization's fine-tuning jobs.",
            required_params => [],
            optional_params => [limit, after, before]
        },
        retrieve_fine_tuning_job => #{
            method => get,
            path => "/fine_tuning/jobs/:fine_tuning_id",
            description => "Get info about a fine-tuning job.",
            required_params => [fine_tuning_id],
            optional_params => []
        },
        cancel_fine_tuning_job => #{
            method => post,
            path => "/fine_tuning/jobs/:fine_tuning_id/cancel",
            description => "Immediately cancel a fine-tune job.",
            required_params => [fine_tuning_id],
            optional_params => []
        },
        list_fine_tuning_events => #{
            method => get,
            path => "/fine_tuning/jobs/:fine_tuning_id/events",
            description => "Get status updates for a fine-tuning job.",
            required_params => [fine_tuning_id],
            optional_params => [after, limit, stream]
        }
    };

%% Images endpoints
get_endpoints(images) ->
    #{
        create_image => #{
            method => post,
            path => "/images/generations",
            description => "Creates an image given a prompt.",
            required_params => [prompt],
            optional_params => [
                model, n, quality, response_format, size, style, user
            ]
        },
        create_image_edit => #{
            method => post,
            path => "/images/edits",
            description => "Creates an edited or extended image given an original image and a prompt.",
            required_params => [image, prompt],
            optional_params => [
                mask, model, n, size, response_format, user
            ]
        },
        create_image_variation => #{
            method => post,
            path => "/images/variations",
            description => "Creates a variation of a given image.",
            required_params => [image],
            optional_params => [
                model, n, response_format, size, user
            ]
        }
    };

%% Audio endpoints
get_endpoints(audio) ->
    #{
        create_transcription => #{
            method => post,
            path => "/audio/transcriptions",
            description => "Transcribes audio into the input language.",
            required_params => [file, model],
            optional_params => [
                language, prompt, response_format, temperature
            ]
        },
        create_translation => #{
            method => post,
            path => "/audio/translations",
            description => "Translates audio into English.",
            required_params => [file, model],
            optional_params => [
                prompt, response_format, temperature
            ]
        },
        create_speech => #{
            method => post, 
            path => "/audio/speech",
            description => "Generates audio from the input text.",
            required_params => [model, input, voice],
            optional_params => [
                response_format, speed
            ]
        }
    };

%% Moderations endpoints
get_endpoints(moderations) ->
    #{
        create_moderation => #{
            method => post,
            path => "/moderations",
            description => "Classifies if text violates OpenAI's Content Policy.",
            required_params => [input],
            optional_params => [model]
        }
    };

%% Assistants endpoints
get_endpoints(assistants) ->
    #{
        create_assistant => #{
            method => post,
            path => "/assistants",
            description => "Create an assistant with a model and optionally tools.",
            required_params => [model],
            optional_params => [
                name, description, instructions, tools, tool_resources,
                file_ids, metadata
            ]
        },
        retrieve_assistant => #{
            method => get,
            path => "/assistants/:assistant_id",
            description => "Retrieves an assistant.",
            required_params => [assistant_id],
            optional_params => []
        },
        modify_assistant => #{
            method => post,
            path => "/assistants/:assistant_id",
            description => "Modifies an assistant.",
            required_params => [assistant_id],
            optional_params => [
                model, name, description, instructions, tools,
                tool_resources, file_ids, metadata
            ]
        },
        delete_assistant => #{
            method => delete,
            path => "/assistants/:assistant_id",
            description => "Delete an assistant.",
            required_params => [assistant_id],
            optional_params => []
        },
        list_assistants => #{
            method => get,
            path => "/assistants",
            description => "Returns a list of assistants.",
            required_params => [],
            optional_params => [
                limit, order, after, before
            ]
        },
        create_assistant_file => #{
            method => post,
            path => "/assistants/:assistant_id/files",
            description => "Creates a file association for an assistant.",
            required_params => [assistant_id, file_id],
            optional_params => []
        },
        retrieve_assistant_file => #{
            method => get,
            path => "/assistants/:assistant_id/files/:file_id",
            description => "Retrieves a file association for an assistant.",
            required_params => [assistant_id, file_id],
            optional_params => []
        },
        delete_assistant_file => #{
            method => delete,
            path => "/assistants/:assistant_id/files/:file_id",
            description => "Delete a file association for an assistant.",
            required_params => [assistant_id, file_id],
            optional_params => []
        },
        list_assistant_files => #{
            method => get,
            path => "/assistants/:assistant_id/files",
            description => "Returns a list of file associations for an assistant.",
            required_params => [assistant_id],
            optional_params => [
                limit, order, after, before
            ]
        }
    };

%% Threads endpoints
get_endpoints(threads) ->
    #{
        create_thread => #{
            method => post,
            path => "/threads",
            description => "Create a thread.",
            required_params => [],
            optional_params => [
                messages, metadata
            ]
        },
        retrieve_thread => #{
            method => get,
            path => "/threads/:thread_id",
            description => "Retrieves a thread.",
            required_params => [thread_id],
            optional_params => []
        },
        modify_thread => #{
            method => post,
            path => "/threads/:thread_id",
            description => "Modifies a thread.",
            required_params => [thread_id],
            optional_params => [
                metadata
            ]
        },
        delete_thread => #{
            method => delete,
            path => "/threads/:thread_id",
            description => "Delete a thread.",
            required_params => [thread_id],
            optional_params => []
        }
    };

%% Messages endpoints
get_endpoints(messages) ->
    #{
        create_message => #{
            method => post,
            path => "/threads/:thread_id/messages",
            description => "Create a message.",
            required_params => [thread_id, role, content],
            optional_params => [
                file_ids, metadata, attachments
            ]
        },
        retrieve_message => #{
            method => get,
            path => "/threads/:thread_id/messages/:message_id",
            description => "Retrieve a message.",
            required_params => [thread_id, message_id],
            optional_params => []
        },
        modify_message => #{
            method => post,
            path => "/threads/:thread_id/messages/:message_id",
            description => "Modifies a message.",
            required_params => [thread_id, message_id],
            optional_params => [
                metadata
            ]
        },
        list_messages => #{
            method => get,
            path => "/threads/:thread_id/messages",
            description => "Returns a list of messages for a given thread.",
            required_params => [thread_id],
            optional_params => [
                limit, order, after, before
            ]
        },
        retrieve_message_file => #{
            method => get,
            path => "/threads/:thread_id/messages/:message_id/files/:file_id",
            description => "Retrieves a message file.",
            required_params => [thread_id, message_id, file_id],
            optional_params => []
        },
        list_message_files => #{
            method => get,
            path => "/threads/:thread_id/messages/:message_id/files",
            description => "Returns a list of message files.",
            required_params => [thread_id, message_id],
            optional_params => [
                limit, order, after, before
            ]
        }
    };

%% Runs endpoints
get_endpoints(runs) ->
    #{
        create_run => #{
            method => post,
            path => "/threads/:thread_id/runs",
            description => "Create a run.",
            required_params => [thread_id, assistant_id],
            optional_params => [
                model, instructions, tools, metadata, stream
            ]
        },
        retrieve_run => #{
            method => get,
            path => "/threads/:thread_id/runs/:run_id",
            description => "Retrieves a run.",
            required_params => [thread_id, run_id],
            optional_params => []
        },
        modify_run => #{
            method => post,
            path => "/threads/:thread_id/runs/:run_id",
            description => "Modifies a run.",
            required_params => [thread_id, run_id],
            optional_params => [
                metadata
            ]
        },
        list_runs => #{
            method => get,
            path => "/threads/:thread_id/runs",
            description => "Returns a list of runs belonging to a thread.",
            required_params => [thread_id],
            optional_params => [
                limit, order, after, before
            ]
        },
        submit_tool_outputs => #{
            method => post,
            path => "/threads/:thread_id/runs/:run_id/submit_tool_outputs",
            description => "Submit tool outputs to run.",
            required_params => [thread_id, run_id, tool_outputs],
            optional_params => [
                stream
            ]
        },
        cancel_run => #{
            method => post,
            path => "/threads/:thread_id/runs/:run_id/cancel",
            description => "Cancels a run that is in_progress.",
            required_params => [thread_id, run_id],
            optional_params => []
        },
        create_thread_and_run => #{
            method => post,
            path => "/threads/runs",
            description => "Create a thread and run it in one request.",
            required_params => [assistant_id],
            optional_params => [
                thread, model, instructions, tools, metadata, stream
            ]
        },
        retrieve_run_step => #{
            method => get,
            path => "/threads/:thread_id/runs/:run_id/steps/:step_id",
            description => "Retrieves a run step.",
            required_params => [thread_id, run_id, step_id],
            optional_params => []
        },
        list_run_steps => #{
            method => get,
            path => "/threads/:thread_id/runs/:run_id/steps",
            description => "Returns a list of run steps belonging to a run.",
            required_params => [thread_id, run_id],
            optional_params => [
                limit, order, after, before
            ]
        }
    };

%% Batch endpoints
get_endpoints(batch) ->
    #{
        create_batch => #{
            method => post,
            path => "/batches",
            description => "Create a batch.",
            required_params => [endpoint],
            optional_params => [
                custom_inputs, input_file_urls, completion_window
            ]
        },
        retrieve_batch => #{
            method => get,
            path => "/batches/:batch_id",
            description => "Retrieve a batch.",
            required_params => [batch_id],
            optional_params => []
        },
        list_batches => #{
            method => get,
            path => "/batches",
            description => "List batches.",
            required_params => [],
            optional_params => [
                limit, after
            ]
        },
        cancel_batch => #{
            method => post,
            path => "/batches/:batch_id/cancel",
            description => "Cancel a batch.",
            required_params => [batch_id],
            optional_params => []
        },
        list_batch_jobs => #{
            method => get,
            path => "/batches/:batch_id/jobs",
            description => "List batch jobs.",
            required_params => [batch_id],
            optional_params => [
                limit, after, status
            ]
        },
        list_all_batch_jobs => #{
            method => get,
            path => "/batch_jobs",
            description => "List all batch jobs.",
            required_params => [],
            optional_params => [
                limit, after, status
            ]
        },
        create_batch_file => #{
            method => post,
            path => "/batches/:batch_id/files",
            description => "Create a batch file.",
            required_params => [batch_id, file_id],
            optional_params => []
        },
        list_batch_files => #{
            method => get,
            path => "/batches/:batch_id/files",
            description => "List batch files.",
            required_params => [batch_id],
            optional_params => [
                limit, after
            ]
        }
    };

%% If an unknown group is requested, return an empty map
get_endpoints(_) ->
    #{}.