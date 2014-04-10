#include <stdlib.h>
#include <alsa/asoundlib.h>

// // This would leave the alsa handle open.
static void init(); // __attribute__((constructor));
static void cleanup(); // __attribute__((destructor));
// // We dont do it anymore because it wastes cpu.

// globals initialized by init()
static snd_pcm_t *capture_handle = NULL;
static const int bytes_per_frame = 2; // SND_PCM_FORMAT_S16_LE, 1 channel
static int err = 0;


// exported functions

int micfill(unsigned char *buf, size_t cnt)
{
	init();
	if(!capture_handle) return err;
	if ((err = snd_pcm_readi (capture_handle, buf, cnt/bytes_per_frame)) != cnt/bytes_per_frame) {
		err = -1;
	}
	cleanup();
	return err;
}

const char *mic_strerror(int errnum) {
	return snd_strerror(errnum);
}


// static functions

static void init()
{
	int step = 0;
	snd_pcm_hw_params_t *hw_params = NULL;
	do {
	step ++;
	if ((err = snd_pcm_open (&capture_handle, "default", SND_PCM_STREAM_CAPTURE, 0)) < 0) {
		break;
	}
	step ++;
	if ((err = snd_pcm_hw_params_malloc (&hw_params)) < 0) {
		break;
	}
	step ++;
	if ((err = snd_pcm_hw_params_any (capture_handle, hw_params)) < 0) {
		break;
	}
	step ++;
	if ((err = snd_pcm_hw_params_set_access (capture_handle, hw_params, SND_PCM_ACCESS_RW_INTERLEAVED)) < 0) {
		break;
	}
	step ++;
	if ((err = snd_pcm_hw_params_set_format (capture_handle, hw_params, SND_PCM_FORMAT_S16_LE)) < 0)
	{
		break;
	}
	unsigned int rate = 44100;
	int dir = 0;
	step ++;
	if ((err = snd_pcm_hw_params_set_rate_near (capture_handle, hw_params, &rate, &dir)) < 0) {
		break;
	}
	step ++;
	if ((err = snd_pcm_hw_params_set_channels (capture_handle, hw_params, 1)) < 0) {
		break;
	}
	step ++;
	if ((err = snd_pcm_hw_params (capture_handle, hw_params)) < 0) {
		break;
	}
	snd_pcm_hw_params_free (hw_params);
	hw_params = NULL;

	err = snd_pcm_nonblock( capture_handle, 0 ); // 0 = block, 1 = nonblock mode, 2 = abort 
	step ++;
	if( err < 0 ) {
	       	break;
	}

	step ++;
	if ((err = snd_pcm_prepare (capture_handle)) < 0) {
		// fprintf (stderr, "cannot prepare audio interface for use (%s)\n", snd_strerror (err));
		break;
	}
	
	} while(0);

	// fprintf(stderr,"initialized mic. step=%d err=%d handle=%p\n", step, err, capture_handle);

	{
		short buf[128] = { 0 };
		// Discard 1 buffer full
		step ++;
		if ((err = snd_pcm_readi (capture_handle, buf, 128)) != 128) {
			if(err>0) err=-1;
		}
	}

	if(hw_params) free(hw_params);
	if(err<0) {
		snd_pcm_close (capture_handle);
		capture_handle = NULL;
	}
}

static void cleanup()
{
	if(capture_handle) {
		snd_pcm_close (capture_handle);
		capture_handle = NULL;
	}
	// fprintf(stderr,"released mic.\n");
}

